module Handler.Tasks where

import Data.List (partition, sortBy)
import Data.Maybe (listToMaybe, fromJust)
import qualified Data.Text as Text
import Data.Time (Day, TimeZone, getCurrentTimeZone)
import Database.Persist.Query.Internal (Update)
import Import
import Data.Aeson.Types (toJSON)
import Util
import Yesod.Auth (requireAuthId)


getTasksR :: Handler RepHtml
getTasksR = do
  userId <- requireAuthId
  tasks <- runDB $ userTasks userId

  estimates <- runDB $ mapM (taskEstimates . entityKey) tasks
  let tasksEstimates :: [(Entity Task, [Entity Estimate])]
      tasksEstimates = tasks `zip` estimates

  timeZone <- userTimeZone
  time <- now
  let taskTodoToday :: Task -> Bool
      taskTodoToday = taskTodo timeZone time
      taskOverdueToday :: Task -> Bool
      taskOverdueToday = taskOverdue timeZone time
      taskDueClass :: Task -> Maybe String
      taskDueClass task | taskOverdueToday task = Just "overdue"
                        | otherwise             = Nothing

  let (unsortedDone, pending) = partition (taskDone . entityVal . fst) tasksEstimates
  let done = reverse $ sortBy (compareBy $ taskDoneAt . entityVal . fst) unsortedDone
  let (active, paused) = partition (taskActive . entityVal . fst) pending
  let (unsortedTodo, unsortedTomorrow) = partition (taskTodoToday . entityVal . fst) active
  let todo = sortBy (compareBy $ taskOrder . entityVal . fst) unsortedTodo
  let tomorrow = sortBy (compareBy $ taskOrder . entityVal . fst) unsortedTomorrow

  let doneByDay :: [(Day, [(Entity Task, [Entity Estimate])])]
      doneByDay = groupByEq (fromJust . taskDoneDay timeZone . entityVal . fst) done

  (newTaskWidget, newTaskEnctype) <- generateFormPost newTaskForm
  (editTaskWidget, editTaskEnctype) <- generateFormPost editTaskForm
  (reorderTaskWidget, reorderTaskEnctype) <- generateFormPost reorderTaskForm

  let taskTr (taskEntity, estimateEntities) = let taskId = entityKey taskEntity; task = entityVal taskEntity in $(widgetFile "tasks/task-tr")
  defaultLayout $ do
      setTitle "tasks"
      addWidget $(widgetFile "tasks") where

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...
  estimatedRemaining :: (Entity Task, [Entity Estimate]) -> Int
  estimatedRemaining (_, []) = 0
  estimatedRemaining (Entity _ task, Entity _ estimate : _) = (estimatePomos estimate - taskPomos task) `max` 0


newTaskForm :: Form NewTask
newTaskForm = renderDivs $ NewTask <$> areq textField "Title" Nothing


editTaskForm :: Form TaskEdit
editTaskForm = renderDivs $ TaskTitleEdit <$> areq textField "Title" Nothing


reorderTaskForm :: Form TaskEdit
reorderTaskForm = renderDivs $ TaskOrderEdit <$> areq intField "Delta" Nothing


postTasksR :: Handler RepHtml
postTasksR = do
  userId <- requireAuthId

  ((result, _), _) <- runFormPost newTaskForm
  case result of
    FormSuccess task -> do
      runDB $ createTaskAtBottom userId task
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
  authedTask taskId
  runDB $ update taskId updates
  redirect route


postCompleteTaskR :: TaskId -> Handler RepHtml
postCompleteTaskR taskId = do
  time <- now
  updateAndRedirectR TasksR [TaskDoneAt =. Just time] taskId

postRestartTaskR :: TaskId -> Handler RepHtml
postRestartTaskR = updateAndRedirectR TasksR [TaskDoneAt =. Nothing]


authedTask :: TaskId -> Handler Task
authedTask taskId = do
    userId <- requireAuthId
    maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
    case maybeAuthedTask of
      Just task -> return $ entityVal task
      Nothing -> redirect TasksR


userTimeZone :: Handler TimeZone
userTimeZone = liftIO getCurrentTimeZone


deleteTaskR :: TaskId -> Handler RepHtml
deleteTaskR taskId = do
  authedTask taskId
  runDB $ delete taskId
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
  authedTask taskId
  runDB $ postponeTask taskId
  redirect TasksR

postUnpostponeTaskR :: TaskId -> Handler RepHtml
postUnpostponeTaskR taskId = do
  authedTask taskId
  runDB $ unpostponeTask taskId
  redirect TasksR


postPauseTaskR :: TaskId -> Handler RepHtml
postPauseTaskR taskId = do
  authedTask taskId
  runDB $ pauseTask taskId
  redirect TasksR

postUnpauseTaskR :: TaskId -> Handler RepHtml
postUnpauseTaskR taskId = do
  authedTask taskId
  runDB $ unpauseTask taskId
  redirect TasksR
