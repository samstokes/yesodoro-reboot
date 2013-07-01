{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Tasks where

import Import

import Data.List (partition, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import System.Locale (defaultTimeLocale)
import Database.Persist.Query.Internal (Update)
import Forms
import Data.Aeson.Types (ToJSON, toJSON)
import Text.Blaze (toMarkup)
import Util
import Util.Angular
import Yesod.Auth (requireAuthId)


instance ToJSON (Entity Task, [Entity Estimate], [Entity Note]) where
  toJSON (Entity taskId task, estimates, notes) = object
    [ "id" .= taskId
    , "task" .= task
    , "estimates" .= estimates, "notes" .= notes]


getTasksR :: Handler RepHtml
getTasksR = do
  Entity userId user <- requireAuth
  let
    features = userFeatureSettings user
    has feature = hasFlag feature features
    scheduleOptions = if has FeatureNonDailySchedules
      then schedules
      else filter (not . nonDaily) schedules
  horizon <- ago $ weeks 2
  tasks <- runDB $ selectUserTasksSince userId horizon [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...
  plans <- runDB $ selectUserPlansSince userId horizon [Desc PlanCreatedAt, Desc PlanDoneAt]

  estimates <- if has FeaturePomos
               then runDB $ mapM (taskEstimates . entityKey) tasks
               else return $ map (const []) tasks
  notes <- if has FeatureNotes
           then runDB $ mapM (taskNotes [Asc NoteCreatedAt] . entityKey) tasks
           else return $ map (const []) tasks
  let tasksEstimatesNotes :: [(Entity Task, [Entity Estimate], [Entity Note])]
      tasksEstimatesNotes = zip3 tasks estimates notes

  timeZone <- currentUserTimeZone
  time <- now
  let taskTodoToday :: Task -> Bool
      taskTodoToday = taskTodo timeZone time
      taskOverdueToday :: Task -> Bool
      taskOverdueToday task = has FeatureOverdueTasks && taskOverdue timeZone time task

  let (unsortedDone, pending) = partition (taskDone . entityVal . fst3) tasksEstimatesNotes
  let done = reverse $ sortBy (comparing $ taskDoneAt . entityVal . fst3) unsortedDone
  let (active, paused) = partition (taskActive . entityVal . fst3) pending
  let (unsortedTodo, unsortedPostponed) = partition (taskTodoToday . entityVal . fst3) active
  let todo = sortBy (comparing $ taskOrder . entityVal . fst3) unsortedTodo
  let postponed = sortBy (comparing $ taskScheduledFor . entityVal . fst3) unsortedPostponed

  let (donePlans, activePlans) = partition (planDone . entityVal) plans

  let doneTasksByDay :: [(Day, [(Entity Task, [Entity Estimate], [Entity Note])])]
      doneTasksByDay = groupByEq (fromJust . taskDoneDay timeZone . entityVal . fst3) done

  let donePlansByDay :: [(Day, [Entity Plan])]
      donePlansByDay = groupByEq (fromJust . planDoneDay timeZone . entityVal) donePlans

  let doneByDay :: [(Day, [Entity Plan], [(Entity Task, [Entity Estimate], [Entity Note])])]
      doneByDay = reverse $ sortBy (comparing fst3) $ unionBothValues donePlansByDay doneTasksByDay

  (newTaskWidget, newTaskEnctype) <- generateFormPost (newTaskForm scheduleOptions)
  (editTaskWidget, editTaskEnctype) <- generateFormPost editTaskForm
  (reorderTaskWidget, reorderTaskEnctype) <- generateFormPost reorderTaskForm

  let
      toggleableFeatures = sort $ filter ((/= FeatureSettings) . fst) features
      featureButtonLabel (feature, False) = Text.pack $ "Enable " ++ featureDescription feature
      featureButtonLabel (feature, True) = Text.pack $ "Disable " ++ featureDescription feature
      taskTr (Entity taskId task, estimateEntities, noteEntities) = do
        maybeExtTask <- lift $ runDB $ taskGetExtTask task
        $(widgetFile "tasks/task-tr")
   in defaultLayout $ do
        title <- lift appTitle
        setTitle $ toMarkup title
        addWidget $(widgetFile "tasks") where

  estimatedRemaining :: (Entity Task, [Entity Estimate], [Entity Note]) -> Int
  estimatedRemaining (_, [], _) = 0
  estimatedRemaining (Entity _ task, Entity _ estimate : _, _) = (estimatePomos estimate - taskPomos task) `max` 0


notesWidget :: TaskId -> [Entity Note] -> Widget
notesWidget taskId notes = do
  widgetId <- lift newIdent
  (newNoteWidget, newNoteEnctype) <- lift $ generateFormPost newNoteForm
  time <- now
  timeZone <- lift currentUserTimeZone

  let renderTime format = formatTime defaultTimeLocale format . utcToLocalTime timeZone
      selector = Text.concat . (["#", widgetId, " "] ++) . pure
      handleSelector = selector ".notes-handle"
      detailSelector = selector ".notes-detail"
      dummyNote = Entity undefined $ Note "dummy" taskId time
      noteWidget (Entity noteId note) = $(widgetFile "notes/note")
    in $(widgetFile "notes")


postTasksR :: Handler RepJson
postTasksR = do
  userId <- requireAuthIdPreventingXsrf

  newTask <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  taskEntity <- runDB $ createTaskAtBottom userId newTask
  jsonToRepJson taskEntity


oneButton :: Text -> Text -> Route App -> Widget
oneButton classes label route = [whamlet|
  <form method=POST action=@{route}>
    <button .#{classes}>#{label}
|]

deleteButton :: Text -> Text -> Route App -> Widget
deleteButton classes label route = [whamlet|
  <form method=POST action=@?{deleteR route}>
    <button .#{classes}>#{label}
|]


updateAndRedirectR :: HasReps a => Route App -> [Update Task] -> TaskId -> Handler a
updateAndRedirectR route updates taskId = do
  _ <- authedTask taskId
  runDB $ update taskId updates
  redirect route


postCompleteTaskR :: TaskId -> Handler RepJson
postCompleteTaskR taskId = do
  taskEntity <- authedTaskPreventingXsrf taskId
  tz <- currentUserTimeZone
  maybeNextTime <- runDB $ completeTask tz taskEntity
  jsonToRepJson maybeNextTime

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = updateAndRedirectR TasksR [TaskDoneAt =. Nothing]


authedTask :: TaskId -> Handler Task
authedTask taskId = do
    userId <- requireAuthId
    maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
    case maybeAuthedTask of
      Just task -> return $ entityVal task
      Nothing -> redirect TasksR


authedTaskPreventingXsrf :: TaskId -> Handler (Entity Task)
authedTaskPreventingXsrf taskId = do
  userId <- requireAuthIdPreventingXsrf
  maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
  case maybeAuthedTask of
    Just task -> return task
    Nothing -> notFound


currentUserTimeZone :: Handler TimeZone
currentUserTimeZone = userTimeZone <$> entityVal <$> requireAuth


deleteTaskR :: TaskId -> Handler RepJson
deleteTaskR taskId = do
  taskEntity <- authedTaskPreventingXsrf taskId
  runDB $ deleteTask taskEntity
  jsonToRepJson True


putTaskR :: TaskId -> Handler RepJson
putTaskR taskId = do
  _ <- authedTask taskId
  tz <- currentUserTimeZone
  ((result, _), _) <- runFormPost editTaskForm
  case result of
    FormSuccess edit -> do
      (updated, _) <- runDB $ updateTask tz edit taskId
      jsonToRepJson $ object [("updated", toJSON updated)]
    _ -> undefined -- TODO


postReorderTaskR :: TaskId -> Handler RepJson
postReorderTaskR taskId = do
  _ <- authedTask taskId
  tz <- currentUserTimeZone
  ((result, _), _) <- runFormPost reorderTaskForm
  case result of
    FormSuccess edit -> do
      (updated, _) <- runDB $ updateTask tz edit taskId
      jsonToRepJson $ object [("updated", toJSON updated)]
    _ -> undefined -- TODO


postPostponeTaskR :: TaskId -> Handler RepJson
postPostponeTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  runDB $ postponeTask (days 1) taskId
  jsonToRepJson True

postUnpostponeTaskR :: TaskId -> Handler RepHtml
postUnpostponeTaskR taskId = do
  _ <- authedTask taskId
  runDB $ unpostponeTask taskId
  redirect TasksR


postPauseTaskR :: TaskId -> Handler RepJson
postPauseTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  runDB $ pauseTask taskId
  jsonToRepJson True

postUnpauseTaskR :: TaskId -> Handler RepHtml
postUnpauseTaskR taskId = do
  _ <- authedTask taskId
  runDB $ unpauseTask taskId
  redirect TasksR


postTaskEstimatesR :: TaskId -> Handler RepHtml
postTaskEstimatesR taskId = do
  _ <- authedTask taskId
  pomosParam <- lookupPostParam "pomos"
  let pomos = do
      param <- maybeToEither "no pomos" pomosParam
      (num, _) <- decimal param
      return num
  case pomos of
    (Right numPomos) -> do
      _ <- runDB $ insert $ Estimate taskId numPomos
      redirect TasksR
    Left msg -> error msg -- TODO


postTaskPomosR :: TaskId -> Handler RepHtml
postTaskPomosR = updateAndRedirectR TasksR [TaskPomos +=. 1]


postTaskNotesR :: TaskId -> Handler RepJson
postTaskNotesR taskId = do
  _ <- authedTask taskId
  ((result, _), _) <- runFormPost newNoteForm
  case result of
    FormSuccess note -> do
      noteEntity <- runDB $ createNote taskId note
      jsonToRepJson noteEntity
    _ -> undefined -- TODO
