{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Tasks where

import Import

import Data.List (partition, sort, zipWith4)
import qualified Data.Text as Text
import Data.Aeson.Types (ToJSON, toJSON)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Text (renderMarkup)
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


getTasksR :: Handler RepHtml
getTasksR = do
  Entity userId user <- requireAuth
  mmsg <- fmap renderMarkup <$> getMessage
  params <- reqGetParams <$> getRequest
  let
    window :: Maybe Integer
    window = read . Text.unpack <$> lookup "days" params
    features = userFeatureSettings user
    has feature = hasFlag feature features
    scheduleOptions = if has FeatureNonDailySchedules
      then schedules
      else filter (not . nonDaily) schedules
  horizon <- ago $ fromMaybe (weeks 2) (days . fromIntegral <$> window)
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

  let (donePlans, activePlans) = partition (planDone . entityVal) plans

  let
      toggleableFeatures = sort $ filter ((/= FeatureSettings) . fst) features
      featureButtonLabel (feature, False) = Text.pack $ "Enable " ++ featureDescription feature
      featureButtonLabel (feature, True) = Text.pack $ "Disable " ++ featureDescription feature
   in defaultLayout $ do
        title <- lift appTitle
        setTitle $ toMarkup title
        addWidget $(widgetFile "tasks") where


postTasksR :: Handler RepJson
postTasksR = do
  userId <- requireNgAuthId

  newTask <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  taskEntity <- runDB $ createTaskAtBottom userId newTask
  jsonToRepJson taskEntity


oneButton :: Text -> Text -> Route App -> Widget
oneButton classes label route = [whamlet|
  <form method=POST action=@{route}>
    <button .#{classes}>#{label}
|]

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
