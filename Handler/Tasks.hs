module Handler.Tasks where

import Data.Function (on)
import Data.List (partition, sortBy)
import Data.Maybe (listToMaybe, fromJust)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import Data.Time (Day, TimeZone, getCurrentTimeZone)
import Database.Persist.Query.Internal (Update)
import Database.Persist.Store (deleteCascade)
import Forms
import Import
import Data.Aeson.Types (toJSON)
import Util
import Yesod.Auth (requireAuthId)


getTasksR :: Handler RepHtml
getTasksR = do
  userId <- requireAuthId
  horizon <- ago $ weeks 2
  tasks <- runDB $ selectUserTasksSince userId horizon [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...
  plans <- runDB $ selectUserActivePlans userId [Desc PlanCreatedAt]

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
  let done = reverse $ sortBy (compare `on` taskDoneAt . entityVal . fst) unsortedDone
  let (active, paused) = partition (taskActive . entityVal . fst) pending
  let (unsortedTodo, unsortedPostponed) = partition (taskTodoToday . entityVal . fst) active
  let todo = sortBy (compare `on` taskOrder . entityVal . fst) unsortedTodo
  let postponed = sortBy (compare `on` taskScheduledFor . entityVal . fst) unsortedPostponed

  let doneByDay :: [(Day, [(Entity Task, [Entity Estimate])])]
      doneByDay = groupByEq (fromJust . taskDoneDay timeZone . entityVal . fst) done

  (newPlanWidget, newPlanEnctype) <- generateFormPost newPlanForm
  (newTaskWidget, newTaskEnctype) <- generateFormPost newTaskForm
  (editTaskWidget, editTaskEnctype) <- generateFormPost editTaskForm
  (reorderTaskWidget, reorderTaskEnctype) <- generateFormPost reorderTaskForm

  let planTr (Entity planId plan) = $(widgetFile "plans/plan-tr")
  let taskTr (Entity taskId task, estimateEntities) = $(widgetFile "tasks/task-tr")
  defaultLayout $ do
      setTitle "tasks"
      addWidget $(widgetFile "tasks") where

  estimatedRemaining :: (Entity Task, [Entity Estimate]) -> Int
  estimatedRemaining (_, []) = 0
  estimatedRemaining (Entity _ task, Entity _ estimate : _) = (estimatePomos estimate - taskPomos task) `max` 0


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


userTimeZone :: Handler TimeZone
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
