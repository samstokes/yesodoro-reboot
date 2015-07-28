{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Tasks where

import Handler
import Import

import Data.List (zipWith4)
import Util
import Util.Angular


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


parseTaskSet :: Handler TaskSet
parseTaskSet = mkTaskSet <*> horizonFromParams where
  mkTaskSet :: Handler (UTCTime -> TaskSet)
  mkTaskSet = do
    query <- lookup "q" . reqGetParams <$> getRequest
    return $ case query of
      Just "today" -> const TasksToday
      Just "postponed" -> const TasksPostponed
      Just "done" -> TasksDoneSince
      Just q -> error $ "unknown task set " ++ show q
      Nothing -> TasksAllSince


getJsonTasksR :: Handler Value
getJsonTasksR = do
  query <- taskSetQuery <$> parseTaskSet
  user <- requireNgAuth
  tasks <- runDB $ query user
  tasksWithChildren <- fetchTaskChildren (entityVal user) tasks
  returnJson tasksWithChildren


fetchTaskChildren :: User -> [Entity Task] -> Handler [TaskWithChildren]
fetchTaskChildren user tasks = zipWith4 TaskWithChildren tasks <$> extTasks <*> estimates <*> notes where
  extTasks = if has FeatureExtTasks
              then runDB $ mapM (taskGetExtTask . entityVal) tasks
              else return $ map (const Nothing) tasks
  estimates = if has FeaturePomos
               then runDB $ mapM (taskEstimates . entityKey) tasks
               else return $ map (const []) tasks
  notes = if has FeatureNotes
           then runDB $ mapM (taskNotes [Asc NoteCreatedAt] . entityKey) tasks
           else return $ map (const []) tasks
  has feature = hasFlag feature features
  features = featureSettings $ Just user


postTasksR :: Handler Value
postTasksR = do
  userId <- requireNgAuthId

  newTask <- requireJsonBody -- TODO error page is HTML, not friendly!
  taskEntity <- runDB $ createTaskAtBottom userId newTask
  returnJson taskEntity


postCompleteTaskR :: TaskId -> Handler Value
postCompleteTaskR taskId = do
  taskEntity <- authedTaskPreventingXsrf taskId
  tz <- currentUserTimeZone
  (completed, recurred) <- runDB $ completeTask tz taskEntity
  returnJson $ object ["completed" .= completed, "recurred" .= recurred]

postRestartTaskR :: TaskId -> Handler Value
postRestartTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  restarted <- runDB $ restartTask taskId
  maybeJson taskId restarted


authedTaskPreventingXsrf :: TaskId -> Handler (Entity Task)
authedTaskPreventingXsrf taskId = do
  userId <- requireNgAuthId
  maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
  case maybeAuthedTask of
    Just task -> return task
    Nothing -> notFound


currentUserTimeZone :: Handler TimeZone
currentUserTimeZone = fromMaybe defaultTimeZone <$> userTimeZone <$> entityVal <$> requireAuth


deleteTaskR :: TaskId -> Handler Value
deleteTaskR taskId = do
  taskEntity <- authedTaskPreventingXsrf taskId
  runDB $ deleteTask taskEntity
  returnJson True


putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
  existingTask <- authedTaskPreventingXsrf taskId
  tz <- currentUserTimeZone
  updatedTask <- ownedTask (taskUser $ entityVal existingTask) <$> requireJsonBody -- TODO error page is HTML, not friendly!
  let edit = existingTask `taskDiff` updatedTask
  (_, updatedTask') <- runDB $ updateTask tz edit taskId
  maybeJson taskId updatedTask'
  where taskDiff _ (Task { taskTitle = newTitle }) = TaskTitleEdit newTitle


patchTaskR :: TaskId -> Handler Value
patchTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  tz <- currentUserTimeZone
  edit <- requireJsonBody -- TODO error page is HTML, not friendly!
  (_, updatedTask) <- runDB $ updateTask tz edit taskId
  maybeJson taskId updatedTask


postPostponeTaskR :: TaskId -> Handler Value
postPostponeTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  postponed <- runDB $ postponeTask (days 1) taskId
  maybeJson taskId postponed

postUnpostponeTaskR :: TaskId -> Handler Value
postUnpostponeTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  unpostponed <- runDB $ unpostponeTask taskId
  maybeJson taskId unpostponed


maybeJson :: ToJSON (Entity val) => Key val -> Maybe (Entity val) -> Handler Value
maybeJson entityId = maybe (error $ "Disappeared out from under me: " ++ show entityId) returnJson


postPauseTaskR :: TaskId -> Handler Value
postPauseTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  paused <- runDB $ pauseTask taskId
  maybeJson taskId paused

postUnpauseTaskR :: TaskId -> Handler Value
postUnpauseTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  unpaused <- runDB $ unpauseTask taskId
  maybeJson taskId unpaused


postTaskEstimatesR :: TaskId -> Handler Value
postTaskEstimatesR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  estimate <- requireJsonBody -- TODO error page is HTML, not friendly!
  estimateId <- runDB . insert $ estimate { estimateTask = taskId }
  returnJson $ Entity estimateId estimate
