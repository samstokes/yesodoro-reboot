{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Tasks where

import Handler
import Import

import Data.List (zipWith4)
import Data.Aeson.Types (ToJSON, toJSON)
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


getJsonTasksR :: Handler RepJson
getJsonTasksR = do
  Entity userId user <- requireNgAuth
  horizon <- horizonFromParams
  tasks <- userTasksSince userId horizon
  tasksWithChildren <- fetchTaskChildren user tasks
  jsonToRepJson tasksWithChildren


userTasksSince :: UserId -> UTCTime -> Handler [Entity Task]
userTasksSince userId horizon = runDB $ selectUserTasksSince userId horizon [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...


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
  features = userFeatureSettings user


postTasksR :: Handler RepJson
postTasksR = do
  userId <- requireNgAuthId

  newTask <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  taskEntity <- runDB $ createTaskAtBottom userId newTask
  jsonToRepJson taskEntity


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


authedTaskPreventingXsrf :: TaskId -> Handler (Entity Task)
authedTaskPreventingXsrf taskId = do
  userId <- requireNgAuthId
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


patchTaskR :: TaskId -> Handler RepJson
patchTaskR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  tz <- currentUserTimeZone
  edit <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  (_, updatedTask) <- runDB $ updateTask tz edit taskId
  maybeJson taskId updatedTask


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


postTaskEstimatesR :: TaskId -> Handler RepJson
postTaskEstimatesR taskId = do
  _ <- authedTaskPreventingXsrf taskId
  estimate <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  estimateId <- runDB . insert $ estimate { estimateTask = taskId }
  jsonToRepJson $ Entity estimateId estimate
