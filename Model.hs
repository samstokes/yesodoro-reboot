{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Model where

import Prelude
import Yesod

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, (.:), (.:?))
import qualified Data.Aeson as J
import Data.Text (Text, pack, unpack)
import Data.Time (Day, TimeZone, UTCTime, NominalDiffTime)
import Data.Typeable (Typeable)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (SqlPersistT)
import Database.Persist (PersistValue(..), deleteCascade)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString)
import Safe (readMay)
import Text.Blaze (ToMarkup, toMarkup)
import Text.Julius (ToJavascript, toJavascript)
import Model.Types
import Util


instance PathPiece Feature where
  toPathPiece = pack . show
  fromPathPiece = readMay . unpack


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll", mkDeleteCascade sqlOnlySettings]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON ExtTask where
  toJSON (ExtTask user extId source url status) = object
    [ "user_id" .= user
    , "ext_id" .= extId
    , "ext_source_name" .= source
    , "ext_url" .= url
    , "ext_status" .= status
    ]


instance ToMarkup (KeyBackend b a) where
  toMarkup = toMarkup . unKey

instance ToJavascript (KeyBackend b a) where
  toJavascript = toJavascript . unKey


instance ToMarkup PersistValue where
  toMarkup (PersistInt64 i) = toMarkup $ show i
  toMarkup _ = undefined

instance ToJavascript PersistValue where
  toJavascript (PersistInt64 i) = toJavascript $ toJSON i
  toJavascript _ = undefined


data NewExtTask = NewExtTask
                   { newExtTaskExtId :: ExternalIdent
                   , newExtTaskExtSourceName :: ExternalSourceName
                   , newExtTaskExtUrl :: Maybe Text
                   , newExtTaskExtStatus :: Maybe Text
                   }
  deriving (Show)


instance FromJSON NewExtTask where
  parseJSON (Object o) = NewExtTask
      <$> (o .: "extId")
      <*> (o .: "extSource")
      <*> (o .:? "extUrl")
      <*> (o .:? "extStatus")
  parseJSON v = fail $ "can't parse external task link: " ++ show v


newtype TaskState = TaskState Text
  deriving (ToMarkup, IsString)


{-selectUserTasksSince :: PersistQuery SqlPersist m => UserId -> UTCTime -> [SelectOpt Task] -> SqlPersist m [Entity Task]-}
selectUserTasksSince userId doneSince = selectList (belongsToUser ++ doneSinceLimit)
  where
    belongsToUser = [TaskUser ==. userId]
    doneSinceLimit = [TaskDoneAt ==. Nothing] ||. [TaskDoneAt >=. Just doneSince]


data NewTask = NewTask
                { newTaskTitle :: Text
                , newTaskSchedule :: Schedule
                , newTaskExt :: Maybe NewExtTask
                }
      deriving (Show)


instance FromJSON NewTask where
  parseJSON (Object o) = NewTask
      <$> (o .: "title")
      <*> (fromMaybe Once <$> o .:? "schedule")
      <*> (o .:? "extTask")
  parseJSON v = fail $ "can't parse task: " ++ show v


{-createTask :: PersistStore SqlPersist m => UserId -> UTCTime -> Int -> NewTask -> SqlPersist m TaskId-}
createTask uid scheduledFor order (NewTask title schedule mExt) = do
  mExtId <- maybeM Nothing (fmap Just . insert) $ newExtTask uid <$> mExt
  insert Task {
      taskUser = uid
    , taskTitle = title
    , taskPomos = 0
    , taskScheduledFor = scheduledFor
    , taskDoneAt = Nothing
    , taskActive = True
    , taskOrder = order
    , taskSchedule = schedule
    , taskExtTask = mExtId
    }

newExtTask :: UserId -> NewExtTask -> ExtTask
newExtTask uid (NewExtTask extId sourceName url status) = ExtTask {
    extTaskUser = uid
  , extTaskExtId = extId
  , extTaskExtSourceName = sourceName
  , extTaskExtUrl = url
  , extTaskExtStatus = status
  }


{-class GetExtTask a where-}
  {-getExtTask :: PersistUnique SqlPersist m => UserId -> a -> SqlPersist m (Maybe (Entity ExtTask))-}

{-instance GetExtTask NewExtTask where-}
getExtTask userId (NewExtTask extId source _ _) = getBy $ UniqueExtTaskSourceId userId source extId


{-syncExtTask :: (PersistQuery SqlPersist m, PersistUnique SqlPersist m) => UserId -> NewExtTask -> SqlPersist m Bool-}
syncExtTask userId extTask = do
  existingExtTaskId <- fmap entityKey <$> getExtTask userId extTask
  maybeM False (fmap fst . updateExtTask extTask) existingExtTaskId


{-updateExtTask :: PersistQuery SqlPersist m => NewExtTask -> ExtTaskId -> SqlPersist m (Bool, Maybe ExtTask)-}
updateExtTask newExt extTaskId = do
    mExtTask <- get extTaskId
    case mExtTask of
      Just extTask -> do
        updated <- updateExtTask' newExt extTask
        (updated,) <$> if updated then get extTaskId else return $ Just extTask
      Nothing -> return (False, Nothing)
  where
    updateExtTask' (NewExtTask _ _ newUrl newStatus) (ExtTask _ _ _ oldUrl oldStatus)
      | newUrl /= oldUrl || newStatus /= oldStatus = update extTaskId [
          ExtTaskExtUrl =. newUrl
        , ExtTaskExtStatus =. newStatus
        ] >> return True
      | otherwise = return False


{-createTaskAtBottom :: (MonadIO m, PersistQuery SqlPersist m) => UserId -> NewTask -> SqlPersist m TaskId-}
createTaskAtBottom userId task = do
  time <- now
  maybeLastTask <- selectFirst [TaskUser ==. userId] [Desc TaskOrder]
  let lastOrder = maybe 0 (taskOrder . entityVal) maybeLastTask
  createTask userId time (succ lastOrder) task


{-findTaskByExtTask :: (PersistUnique SqlPersist m, PersistQuery SqlPersist m, GetExtTask extTask) => UserId -> extTask -> SqlPersist m (Maybe (Entity Task))-}
findTaskByExtTask userId extTask = do
  mExtTask <- getExtTask userId extTask
  case mExtTask of
    Just (Entity extTaskId _) -> selectFirst [TaskUser ==. userId, TaskExtTask ==. Just extTaskId] []
    Nothing -> return Nothing


{-completeTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> Entity Task -> SqlPersist m (Maybe TaskId)-}
completeTask tz taskEntity = do
    time <- now
    maybeNextTime <- recurTask tz time taskEntity
    update (entityKey taskEntity) [TaskDoneAt =. Just time]
    return maybeNextTime


cloneTask :: Int -> Task -> Task
cloneTask order task = Task {
    taskUser = taskUser task
  , taskTitle = taskTitle task
  , taskPomos = 0
  , taskScheduledFor = taskScheduledFor task
  , taskDoneAt = Nothing
  , taskActive = True
  , taskOrder = order
  , taskSchedule = taskSchedule task
  , taskExtTask = Nothing
  }


{-duplicateTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> UTCTime -> Entity Task -> SqlPersist m TaskId-}
duplicateTask tz scheduledFor taskEntity = do
    let (taskId, task) = (entityKey taskEntity, entityVal taskEntity)

    maybeFirstTask <- selectFirst [TaskUser ==. taskUser task] [Asc TaskOrder]
    let firstOrder = maybe 0 (taskOrder . entityVal) maybeFirstTask

    let dupeTask = cloneTask (pred firstOrder) task
    dupeTaskId <- insert $ dupeTask { taskScheduledFor = scheduledFor }

    -- TODO can this loop infinitely?
    untilM ((>= taskOrder task) . taskOrder . snd) (reorderTask Down tz) (dupeTaskId, dupeTask)

    _ <- copyFirstEstimate taskId dupeTaskId

    return dupeTaskId
  where
    {-copyFirstEstimate :: PersistQuery SqlPersist m => TaskId -> TaskId -> SqlPersist m [EstimateId]-}
    copyFirstEstimate taskId newTaskId = do
      firstEstimate <- take 1 <$> taskEstimates taskId
      mapM (copyEstimate newTaskId . entityVal) firstEstimate
    {-copyEstimate :: PersistQuery SqlPersist m => TaskId -> Estimate -> SqlPersist m EstimateId-}
    copyEstimate newTaskId (Estimate _ pomos) = insert $ Estimate newTaskId pomos


{-deleteTask :: PersistQuery SqlPersist m => Entity Task -> SqlPersist m ()-}
deleteTask (Entity taskId task) = do
  deleteCascade taskId
  maybeM () delete $ taskExtTask task


{-recurTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> UTCTime -> Entity Task -> SqlPersist m (Maybe TaskId)-}
recurTask tz time taskEntity = do
    let recurrence = scheduleRecurrence $ taskSchedule (entityVal taskEntity)
    maybe (return Nothing) (applyRecurrence >=> return . Just) recurrence
  where
    {-applyRecurrence :: (MonadIO m, PersistQuery SqlPersist m) => NominalDiffTime -> SqlPersist m TaskId-}
    applyRecurrence recurrence = do
      newTaskId <- duplicateTask tz time taskEntity
      postponeTask recurrence newTaskId
      return newTaskId


data TaskEdit = TaskTitleEdit { taskTitleAfter :: Text }
              | TaskOrderEdit { taskOrderDelta :: Int }
              | TaskSyncEdit { taskSyncTask :: NewTask }
              deriving (Show)


{-updateTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> TaskEdit -> TaskId -> SqlPersist m (Bool, Maybe Task)-}
updateTask tz edit taskId = do
    mtask <- get taskId
    case mtask of
      Just task -> do
        updated <- updateTask' edit task
        (updated,) <$> if updated then get taskId else return $ Just task
      Nothing -> return (False, Nothing)
  where
    updateTask' (TaskTitleEdit title) task
      | taskTitle task /= title = update taskId [TaskTitle =. title] >> return True
      | otherwise               = return False
    updateTask' (TaskOrderEdit delta) task = reorderTaskN delta tz (taskId, task)
    updateTask' (TaskSyncEdit (NewTask newTitle _ _)) task = updateTask' (TaskTitleEdit newTitle) task


data Direction = Up | Down deriving (Show, Enum, Bounded)

{-nextTask :: (MonadIO m, PersistQuery SqlPersist m) => Direction -> TimeZone -> Task -> SqlPersist m (Maybe (Entity Task))-}
nextTask direction tz task = do
  endOfDay <- liftIO $ endOfToday tz
  selectFirst
    [ TaskUser ==. taskUser task
    , orderConstraint direction TaskOrder (taskOrder task)
    , TaskDoneAt ==. Nothing
    , scheduledForConstraint endOfDay TaskScheduledFor
    , TaskActive ==. True
    ] [order direction TaskOrder]
  where
    orderConstraint Up = (<.)
    orderConstraint Down = (>.)
    order Up = Desc
    order Down = Asc
    scheduledForConstraint endOfDay | taskScheduledFor task <= endOfDay = (<=. endOfDay)
                                    | otherwise                           = (>. endOfDay)

{-reorderTask :: (MonadIO m, PersistQuery SqlPersist m) => Direction -> TimeZone -> (TaskId, Task) -> SqlPersist m (TaskId, Task)-}
reorderTask direction tz (taskId, task) = do
  maybeNext <- nextTask direction tz task
  case maybeNext of
    Nothing -> return (taskId, task)
    Just (Entity nextId next) -> do
      update taskId [TaskOrder =. minBound] -- temporary value
      update nextId [TaskOrder =. taskOrder task]
      update taskId [TaskOrder =. taskOrder next]
      return (taskId, task { taskOrder = taskOrder next })


{-reorderTaskN :: (MonadIO m, PersistQuery SqlPersist m) => Int -> TimeZone -> (TaskId, Task) -> SqlPersist m Bool-}
reorderTaskN delta tz task
  | delta > 0 = foldTimesM delta (reorder Down) task >> return True
  | delta < 0 = foldTimesM (-delta) (reorder Up) task >> return True
  | otherwise = return False
  where
    reorder direction = reorderTask direction tz


taskDone :: Task -> Bool
taskDone = isJust . taskDoneAt


taskHasPomos :: Task -> Bool
taskHasPomos = (> 0) . taskPomos


taskTodo :: TimeZone -> UTCTime -> Task -> Bool
taskTodo tz moment task = taskActive task && taskScheduledForDay tz task <= today
  where today = utcToLocalDay tz moment

taskOverdue :: TimeZone -> UTCTime -> Task -> Bool
taskOverdue tz moment task = not (taskDone task) && taskActive task && taskScheduledForDay tz task < today
  where today = utcToLocalDay tz moment

taskDoneDay :: TimeZone -> Task -> Maybe Day
taskDoneDay tz = fmap (utcToLocalDay tz) . taskDoneAt

taskScheduledForDay :: TimeZone -> Task -> Day
taskScheduledForDay tz = utcToLocalDay tz . taskScheduledFor

taskState :: Task -> TaskState
taskState task = if taskDone task then "done" else "pending"

{-taskEstimates :: PersistQuery SqlPersist m => TaskId -> SqlPersist m [Entity Estimate]-}
taskEstimates taskId = selectList [EstimateTask ==. taskId] []

estimateOptions :: [Int]
estimateOptions = 0 : [2 ^ x | x <- [0 .. 3] :: [Int]]


{-postponeTask :: (MonadIO m, PersistQuery SqlPersist m) => NominalDiffTime -> TaskId -> SqlPersist m ()-}
postponeTask postponement taskId = do
  postponed <- hence postponement
  update taskId [TaskScheduledFor =. postponed]

{-unpostponeTask :: (MonadIO m, PersistQuery SqlPersist m) => TaskId -> SqlPersist m ()-}
unpostponeTask taskId = do
  time <- now
  update taskId [TaskScheduledFor =. time]


{-pauseTask :: PersistQuery SqlPersist m => TaskId -> SqlPersist m ()-}
pauseTask taskId = update taskId [TaskActive =. False]

{-unpauseTask :: (MonadIO m, PersistQuery SqlPersist m) => TaskId -> SqlPersist m ()-}
unpauseTask taskId = update taskId [TaskActive =. True] >> unpostponeTask taskId


{-taskGetExtTask :: PersistStore SqlPersist m => Task -> SqlPersist m (Maybe ExtTask)-}
taskGetExtTask task = maybeM Nothing get $ taskExtTask task


{-allExtTasksForSource :: PersistQuery SqlPersist m => UserId -> ExternalSourceName -> SqlPersist m [Entity ExtTask]-}
allExtTasksForSource userId source = selectList [
    ExtTaskUser ==. userId
  , ExtTaskExtSourceName ==. source
  ] []


userFeatureSettings :: User -> Flags Feature
userFeatureSettings = defaultMissing . userFeatures


{-toggleUserFeature :: PersistQuery SqlPersist m => Feature -> Entity User -> SqlPersist m ()-}
toggleUserFeature feature (Entity userId user) = do
  let features = toggleFlag feature $ userFeatures user
  update userId [UserFeatures =. features]
