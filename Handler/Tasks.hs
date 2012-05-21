module Handler.Tasks where

import Data.List (partition, sortBy)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTimeZone)
import Import
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

  let taskTr taskEntity = let taskId = entityKey taskEntity; task = entityVal taskEntity in $(widgetFile "tasks/task-tr")
  defaultLayout $ do
      setTitle "tasks"
      addWidget $(widgetFile "tasks") where

  userTasks userId = selectList [TaskUser ==. userId] [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...


newTaskForm :: Form NewTask
newTaskForm = renderDivs $ NewTask <$> areq textField "Title" Nothing


postTasksR :: Handler RepHtml
postTasksR = do
  userId <- requireAuthId

  ((result, _), _) <- runFormPost newTaskForm
  case result of
    FormSuccess task -> do
      runDB $ createTaskAtBottom userId task
      redirect TasksR
    _ -> undefined -- TODO
