module Handler.Tasks where

import Data.List (partition, sortBy)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTimeZone)
import Database.Persist.Query.Internal (Update)
import Import
import Data.Aeson.Types (toJSON)
import Util
import Yesod.Auth (requireAuthId)


getTasksR :: Handler RepHtml
getTasksR = do
  userId <- requireAuthId
  tasks <- runDB $ userTasks userId

  timeZone <- liftIO getCurrentTimeZone
  time <- now
  let taskTodoToday :: Task -> Bool
      taskTodoToday = taskTodo timeZone time
      taskOverdueToday :: Task -> Bool
      taskOverdueToday = taskOverdue timeZone time
      taskDueClass :: Task -> Maybe String
      taskDueClass task | taskOverdueToday task = Just "overdue"
                        | otherwise             = Nothing

  let (unsortedDone, pending) = partition (taskDone . entityVal) tasks
  let done = reverse $ sortBy (compareBy $ taskDoneAt . entityVal) unsortedDone
  let (active, paused) = partition (taskActive . entityVal) pending
  let (unsortedTodo, unsortedTomorrow) = partition (taskTodoToday . entityVal) active
  let todo = sortBy (compareBy $ taskOrder . entityVal) unsortedTodo
  let tomorrow = sortBy (compareBy $ taskOrder . entityVal) unsortedTomorrow

  let doneByDay = groupByEq (fromJust . taskDoneDay timeZone . entityVal) done

  (newTaskWidget, newTaskEnctype) <- generateFormPost newTaskForm
  (editTaskWidget, editTaskEnctype) <- generateFormPost editTaskForm

  let taskTr taskEntity = let taskId = entityKey taskEntity; task = entityVal taskEntity in $(widgetFile "tasks/task-tr")
  defaultLayout $ do
      setTitle "tasks"
      addWidget $(widgetFile "tasks") where

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...


newTaskForm :: Form NewTask
newTaskForm = renderDivs $ NewTask <$> areq textField "Title" Nothing


editTaskForm :: Form TaskEdit
editTaskForm = renderDivs $ TaskTitleEdit <$> areq textField "Title" Nothing


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


deleteTaskR :: TaskId -> Handler RepHtml
deleteTaskR taskId = do
  authedTask taskId
  runDB $ delete taskId
  redirect TasksR


putTaskR :: TaskId -> Handler RepJson
putTaskR taskId = do
  task <- authedTask taskId
  ((result, _), _) <- runFormPost editTaskForm
  case result of
    FormSuccess edit -> do
      updated <- runDB $ updateTask edit (taskId, task)
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
