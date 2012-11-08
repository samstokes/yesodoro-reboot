module Handler.Tasks where

import Control.Monad.IO.Class (MonadIO)
import Data.List (partition, sortBy)
import Data.Maybe (listToMaybe, fromJust)
import Data.Ord (comparing)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import Data.Time (Day, TimeZone, formatTime, getCurrentTimeZone, utcToLocalTime)
import System.Locale (defaultTimeLocale)
import Database.Persist.Query.Internal (Update)
import Database.Persist.Store (deleteCascade)
import Forms
import Import
import Data.Aeson.Types (toJSON)
import Util
import Yesod.Auth (requireAuthId)
import Yesod.Form.Jquery (YesodJquery(..))


getTasksR :: Handler RepHtml
getTasksR = do
  userId <- requireAuthId
  horizon <- ago $ weeks 2
  tasks <- runDB $ selectUserTasksSince userId horizon [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...
  plans <- runDB $ selectUserPlansSince userId horizon [Desc PlanCreatedAt, Desc PlanDoneAt]

  estimates <- runDB $ mapM (taskEstimates . entityKey) tasks
  notes <- runDB $ mapM (taskNotes [Asc NoteCreatedAt] . entityKey) tasks
  let tasksEstimatesNotes :: [(Entity Task, [Entity Estimate], [Entity Note])]
      tasksEstimatesNotes = zip3 tasks estimates notes

  timeZone <- userTimeZone
  time <- now
  let taskTodoToday :: Task -> Bool
      taskTodoToday = taskTodo timeZone time
      taskOverdueToday :: Task -> Bool
      taskOverdueToday = taskOverdue timeZone time

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

  (newPlanWidget, newPlanEnctype) <- generateFormPost newPlanForm
  (newTaskWidget, newTaskEnctype) <- generateFormPost newTaskForm
  (editTaskWidget, editTaskEnctype) <- generateFormPost editTaskForm
  (reorderTaskWidget, reorderTaskEnctype) <- generateFormPost reorderTaskForm

  let planTr (Entity planId plan) = $(widgetFile "plans/plan-tr")
  let taskTr (Entity taskId task, estimateEntities, noteEntities) = $(widgetFile "tasks/task-tr")
  defaultLayout $ do
      setTitle "tasks"
      addWidget $(widgetFile "tasks") where

  estimatedRemaining :: (Entity Task, [Entity Estimate], [Entity Note]) -> Int
  estimatedRemaining (_, [], _) = 0
  estimatedRemaining (Entity _ task, Entity _ estimate : _, _) = (estimatePomos estimate - taskPomos task) `max` 0


notesWidget :: TaskId -> [Entity Note] -> Widget
notesWidget taskId notes = do
  widgetId <- lift newIdent
  (newNoteWidget, newNoteEnctype) <- lift $ generateFormPost newNoteForm
  time <- now
  timeZone <- userTimeZone
  let renderTime format = formatTime defaultTimeLocale format . utcToLocalTime timeZone
  let dummyNote = Entity undefined $ Note "dummy" taskId time
  let noteWidget (Entity noteId note) = $(widgetFile "notes/note")
  $(widgetFile "notes")


postTasksR :: Handler RepHtml
postTasksR = do
  userId <- requireAuthId

  ((result, _), _) <- runFormPost newTaskForm
  case result of
    FormSuccess task -> do
      _ <- runDB $ createTaskAtBottom userId task
      redirect TasksR
    _ -> undefined -- TODO


oneButton :: Text -> Route App -> Widget
oneButton label route = [whamlet|
  <form method=POST action=@{route}>
    <button>#{label}
|]

deleteButton :: Text -> Route App -> Widget
deleteButton label route = [whamlet|
  <form method=POST action=@?{deleteR route}>
    <button>#{label}
|]

setTaskDonenessRoute :: Task -> TaskId -> Route App
setTaskDonenessRoute task | taskDone task = RestartTaskR
                          | otherwise     = CompleteTaskR

setTaskDonenessButton :: TaskId -> Task -> Widget
setTaskDonenessButton taskId task = oneButton action $ route taskId
  where action = taskDonenessActionName task
        route = setTaskDonenessRoute task


updateAndRedirectR :: HasReps a => Route App -> [Update Task] -> TaskId -> Handler a
updateAndRedirectR route updates taskId = do
  _ <- authedTask taskId
  runDB $ update taskId updates
  redirect route


postCompleteTaskR :: TaskId -> Handler RepHtml
postCompleteTaskR taskId = do
  task <- authedTask taskId
  tz <- userTimeZone
  _ <- runDB $ completeTask tz (Entity taskId task)
  redirect TasksR

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = updateAndRedirectR TasksR [TaskDoneAt =. Nothing]


authedTask :: TaskId -> Handler Task
authedTask taskId = do
    userId <- requireAuthId
    maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
    case maybeAuthedTask of
      Just task -> return $ entityVal task
      Nothing -> redirect TasksR


userTimeZone :: MonadIO m => m TimeZone
userTimeZone = liftIO getCurrentTimeZone


deleteTaskR :: TaskId -> Handler RepHtml
deleteTaskR taskId = do
  _ <- authedTask taskId
  runDB $ deleteCascade taskId
  redirect TasksR


putTaskR :: TaskId -> Handler RepJson
putTaskR taskId = do
  task <- authedTask taskId
  tz <- userTimeZone
  ((result, _), _) <- runFormPost editTaskForm
  case result of
    FormSuccess edit -> do
      updated <- runDB $ updateTask tz edit (taskId, task)
      jsonToRepJson $ object [("updated", toJSON updated)]
    _ -> undefined -- TODO


postReorderTaskR :: TaskId -> Handler RepJson
postReorderTaskR taskId = do
  task <- authedTask taskId
  tz <- userTimeZone
  ((result, _), _) <- runFormPost reorderTaskForm
  case result of
    FormSuccess edit -> do
      updated <- runDB $ updateTask tz edit (taskId, task)
      jsonToRepJson $ object [("updated", toJSON updated)]
    _ -> undefined -- TODO


postPostponeTaskR :: TaskId -> Handler RepHtml
postPostponeTaskR taskId = do
  _ <- authedTask taskId
  runDB $ postponeTask (days 1) taskId
  redirect TasksR

postUnpostponeTaskR :: TaskId -> Handler RepHtml
postUnpostponeTaskR taskId = do
  _ <- authedTask taskId
  runDB $ unpostponeTask taskId
  redirect TasksR


postPauseTaskR :: TaskId -> Handler RepHtml
postPauseTaskR taskId = do
  _ <- authedTask taskId
  runDB $ pauseTask taskId
  redirect TasksR

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


data ExpandyState = Collapsed | Expanded

expandyIndicator :: ExpandyState -> Text
expandyIndicator Collapsed = "☞"
expandyIndicator Expanded = "☟"

expandy :: YesodJquery master => ExpandyState -> Text -> Text -> Text -> GWidget sub master ()
expandy initialState parentId handleSelector targetSelector = do
  master <- lift getYesod
  addScriptEither $ urlJqueryJs master

  widgetId <- lift newIdent

  $(widgetFile "expandy")
