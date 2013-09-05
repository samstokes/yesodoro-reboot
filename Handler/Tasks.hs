{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Tasks where

import Import

import Data.List (partition, sort, sortBy, zipWith4)
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


data TaskWithChildren = TaskWithChildren
  { twcTask :: Entity Task
  , twcExtTask :: Maybe ExtTask
  , twcEstimates :: [Entity Estimate]
  , twcNotes :: [Entity Note]
  }


instance ToJSON TaskWithChildren where
  toJSON (TaskWithChildren (Entity taskId task) extTask estimates notes) = object
    [ "id" .= taskId
    , "task" .= task
    , "ext_task" .= extTask
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

  extTasks <- if has FeatureExtTasks
              then runDB $ mapM (taskGetExtTask . entityVal) tasks
              else return $ map (const Nothing) tasks
  estimates <- if has FeaturePomos
               then runDB $ mapM (taskEstimates . entityKey) tasks
               else return $ map (const []) tasks
  notes <- if has FeatureNotes
           then runDB $ mapM (taskNotes [Asc NoteCreatedAt] . entityKey) tasks
           else return $ map (const []) tasks
  let tasksWithChildren :: [TaskWithChildren]
      tasksWithChildren = zipWith4 TaskWithChildren tasks extTasks estimates notes

  timeZone <- currentUserTimeZone
  time <- now
  let taskTodoToday :: Task -> Bool
      taskTodoToday = taskTodo timeZone time
      taskOverdueToday :: Task -> Bool
      taskOverdueToday task = has FeatureOverdueTasks && taskOverdue timeZone time task

  let (unsortedDone, pending) = partition (taskDone . entityVal . twcTask) tasksWithChildren
  let done = reverse $ sortBy (comparing $ taskDoneAt . entityVal . twcTask) unsortedDone
  let (active, paused) = partition (taskActive . entityVal . twcTask) pending
  let (unsortedTodo, unsortedPostponed) = partition (taskTodoToday . entityVal . twcTask) active
  let todo = sortBy (comparing $ taskOrder . entityVal . twcTask) unsortedTodo
  let postponed = sortBy (comparing $ taskScheduledFor . entityVal . twcTask) unsortedPostponed

  let (donePlans, activePlans) = partition (planDone . entityVal) plans

  let doneTasksByDay :: [(Day, [TaskWithChildren])]
      doneTasksByDay = groupByEq (fromJust . taskDoneDay timeZone . entityVal . twcTask) done

  let donePlansByDay :: [(Day, [Entity Plan])]
      donePlansByDay = groupByEq (fromJust . planDoneDay timeZone . entityVal) donePlans

  let doneByDay :: [(Day, [Entity Plan], [TaskWithChildren])]
      doneByDay = reverse $ sortBy (comparing fst3) $ unionBothValues donePlansByDay doneTasksByDay

  (newTaskWidget, newTaskEnctype) <- generateFormPost (newTaskForm scheduleOptions)
  (editTaskWidget, editTaskEnctype) <- generateFormPost editTaskForm
  (reorderTaskWidget, reorderTaskEnctype) <- generateFormPost reorderTaskForm

  let
      toggleableFeatures = sort $ filter ((/= FeatureSettings) . fst) features
      featureButtonLabel (feature, False) = Text.pack $ "Enable " ++ featureDescription feature
      featureButtonLabel (feature, True) = Text.pack $ "Disable " ++ featureDescription feature
      taskTr (TaskWithChildren {
                twcTask = Entity taskId task
              , twcExtTask = maybeExtTask
              , twcEstimates = estimateEntities
              , twcNotes = noteEntities
              }) = $(widgetFile "tasks/task-tr")
   in defaultLayout $ do
        title <- lift appTitle
        setTitle $ toMarkup title
        addWidget $(widgetFile "tasks") where

  estimatedRemaining :: TaskWithChildren -> Int
  estimatedRemaining (TaskWithChildren _ _ [] _) = 0
  estimatedRemaining (TaskWithChildren (Entity _ task) _ (Entity _ estimate : _) _) = (estimatePomos estimate - taskPomos task) `max` 0


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
  (completed, recurred) <- runDB $ completeTask tz taskEntity
  jsonToRepJson $ object ["completed" .= completed, "recurred" .= recurred]

postRestartTaskR :: TaskId -> Handler RepJson
postRestartTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  restarted <- runDB $ restartTask taskId
  maybeJson taskId restarted


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
  existingTask <- authedTaskPreventingXsrf taskId
  tz <- currentUserTimeZone
  updatedTask <- ownedTask (taskUser $ entityVal existingTask) <$> parseJsonBody_ -- TODO error page is HTML, not friendly!
  let edit = existingTask `taskDiff` updatedTask
  (_, updatedTask') <- runDB $ updateTask tz edit taskId
  maybeJson taskId updatedTask'
  where taskDiff _ (Task { taskTitle = newTitle }) = TaskTitleEdit newTitle


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
  postponed <- runDB $ postponeTask (days 1) taskId
  maybeJson taskId postponed

postUnpostponeTaskR :: TaskId -> Handler RepJson
postUnpostponeTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  unpostponed <- runDB $ unpostponeTask taskId
  maybeJson taskId unpostponed


maybeJson :: ToJSON (Entity val) => Key b val -> Maybe (Entity val) -> Handler RepJson
maybeJson entityId = maybe (error $ "Disappeared out from under me: " ++ show entityId) jsonToRepJson


postPauseTaskR :: TaskId -> Handler RepJson
postPauseTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  paused <- runDB $ pauseTask taskId
  maybeJson taskId paused

postUnpauseTaskR :: TaskId -> Handler RepJson
postUnpauseTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  unpaused <- runDB $ unpauseTask taskId
  maybeJson taskId unpaused


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
