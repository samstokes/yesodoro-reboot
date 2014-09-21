{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Model where

import Types

import Prelude
import Yesod

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad ((>=>))
import Data.Aeson ((.:?))
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Time (Day, NominalDiffTime, TimeZone, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (SqlPersistT)
import Data.Maybe (fromMaybe)
import Text.Blaze (ToMarkup, toMarkup)
import Text.Julius (ToJavascript, toJavascript)
import Util


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll", mkDeleteCascade sqlOnlySettings]
    $(persistFileWith lowerCaseSettings "config/models")


instance ToJSON Estimate where
  toJSON (Estimate taskId pomos) = object ["task_id" .= taskId, "pomos" .= pomos]

instance ToJSON (Entity Estimate) where
  toJSON (Entity k e) = object ["id" .= k, "estimate" .= e]

instance FromJSON Estimate where
  parseJSON (Object o) = Estimate
    <$> (o .: "task_id")
    <*> (o .: "pomos")
  parseJSON v = fail $ "can't parse estimate: " ++ show v


instance ToJSON Task where
  toJSON (Task
    { taskUser = user
    , taskTitle = title
    , taskPomos = pomos
    , taskScheduledFor = scheduledFor
    , taskDoneAt = doneAt
    , taskActive = active
    , taskOrder = order
    , taskSchedule = schedule
    , taskExtTask = extTask
    }) = object
    [ "user_id" .= user
    , "title" .= title
    , "pomos" .= pomos
    , "scheduled_for" .= scheduledFor
    , "done_at" .= doneAt
    , "active" .= active
    , "order" .= order
    , "schedule" .= schedule
    , "ext_task" .= extTask
    ]

instance ToJSON (Entity Task) where
  toJSON (Entity k t) = object ["id" .= k, "task" .= t]


instance ToJSON ExtTask where
  toJSON (ExtTask user extId source url status) = object
    [ "user_id" .= user
    , "ext_id" .= extId
    , "ext_source_name" .= source
    , "ext_url" .= url
    , "ext_status" .= status
    ]


instance ToMarkup (KeyBackend a b) where
  toMarkup = toMarkup . unKey

instance ToJavascript (KeyBackend a b) where
  toJavascript = toJavascript . unKey


instance ToMarkup PersistValue where
  toMarkup (PersistInt64 i) = toMarkup $ show i
  toMarkup _ = undefined

instance ToJavascript PersistValue where
  toJavascript (PersistInt64 i) = toJavascript $ toJSON i
  toJavascript _ = undefined


data TaskSet = TasksToday | TasksPostponed | TasksDoneSince { tasksDoneHorizon :: UTCTime } | TasksAllSince { tasksAllHorizon :: UTCTime }


taskSetQuery :: (Applicative m, MonadIO m, PersistQuery (SqlPersistT m)) => TaskSet -> Entity User -> SqlPersistT m [Entity Task]
taskSetQuery TasksToday (Entity userId user) = filterTodo <*> selectUserTasks userId [TaskDoneAt ==. Nothing, TaskActive ==. True] [Asc TaskOrder]
  -- for now, filter in code - push into SQL once we can do more complex queries
  where filterTodo = do
          moment <- now
          let tz = userTimeZone user
          return $ filter $ taskTodo tz moment . entityVal

taskSetQuery TasksPostponed (Entity userId user) = filterPostponed <*> selectUserTasks userId [TaskDoneAt ==. Nothing, TaskActive ==. True] [Asc TaskScheduledFor]
  -- for now, filter in code - push into SQL once we can do more complex queries
  where filterPostponed = do
          moment <- now
          let tz = userTimeZone user
          return $ filter $ taskPostponed tz moment . entityVal

taskSetQuery (TasksDoneSince horizon) (Entity userId _) = selectUserTasks userId [TaskDoneAt >=. Just horizon] [Desc TaskDoneAt]
taskSetQuery (TasksAllSince horizon) (Entity userId _) = selectUserTasks userId doneSinceLimit [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...
  where
    doneSinceLimit = [TaskDoneAt ==. Nothing] ||. [TaskDoneAt >=. Just horizon]


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


selectUserTasks :: PersistQuery (SqlPersistT m) => UserId -> [Filter Task] -> [SelectOpt Task] -> SqlPersistT m [Entity Task]
selectUserTasks userId conditions = selectList (belongsToUser : conditions)
  where belongsToUser = TaskUser ==. userId


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


data OwnedTask = OwnedTask
  { ownedTaskTitle :: Text
  , ownedTaskPomos :: Int
  , ownedTaskScheduledFor :: UTCTime
  , ownedTaskDoneAt :: Maybe UTCTime
  , ownedTaskActive :: Bool
  , ownedTaskOrder :: Int
  , ownedTaskSchedule :: Schedule
  , ownedTaskExtTask :: Maybe ExtTaskId
  }


instance FromJSON OwnedTask where
  parseJSON (Object o) = OwnedTask
    <$> (o .: "title")
    <*> (fromMaybe 0 <$> o .:? "pomos")
    <*> (o .: "scheduled_for")
    <*> (o .:? "done_at")
    <*> (o .: "active")
    <*> (o .: "order")
    <*> (o .: "schedule")
    <*> (o .:? "ext_task")
  parseJSON v = fail $ "can't parse task: " ++ show v


ownedTask :: UserId -> OwnedTask -> Task
ownedTask userId (OwnedTask
  { ownedTaskTitle = title
  , ownedTaskPomos = pomos
  , ownedTaskScheduledFor = scheduledFor
  , ownedTaskDoneAt = doneAt
  , ownedTaskActive = active
  , ownedTaskOrder = order
  , ownedTaskSchedule = schedule
  , ownedTaskExtTask = extTask
  }) = Task
  { taskUser = userId
  , taskTitle = title
  , taskPomos = pomos
  , taskScheduledFor = scheduledFor
  , taskDoneAt = doneAt
  , taskActive = active
  , taskOrder = order
  , taskSchedule = schedule
  , taskExtTask = extTask
  }


createTask :: (Functor m, PersistStore (SqlPersistT m)) => UserId -> UTCTime -> Int -> NewTask -> SqlPersistT m (Entity Task)
createTask uid scheduledFor order (NewTask title schedule mExt) = do
  mExtId <- maybeM Nothing (fmap Just . insert) $ newExtTask uid <$> mExt
  let task = Task {
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
  taskId <- insert task
  return $ Entity taskId task

newExtTask :: UserId -> NewExtTask -> ExtTask
newExtTask uid (NewExtTask extId sourceName url status) = ExtTask {
    extTaskUser = uid
  , extTaskExtId = extId
  , extTaskExtSourceName = sourceName
  , extTaskExtUrl = url
  , extTaskExtStatus = status
  }


class GetExtTask a where
  getExtTask :: PersistUnique (SqlPersistT m) => UserId -> a -> SqlPersistT m (Maybe (Entity ExtTask))

instance GetExtTask NewExtTask where
  getExtTask userId (NewExtTask extId source _ _) = getBy $ UniqueExtTaskSourceId userId source extId


syncExtTask :: (Functor m, PersistQuery (SqlPersistT m), PersistUnique (SqlPersistT m)) => UserId -> NewExtTask -> SqlPersistT m Bool
syncExtTask userId extTask = do
  existingExtTaskId <- fmap entityKey <$> getExtTask userId extTask
  maybeM False (fmap fst . updateExtTask extTask) existingExtTaskId


updateExtTask :: (Functor m, PersistQuery (SqlPersistT m)) => NewExtTask -> ExtTaskId -> SqlPersistT m (Bool, Maybe ExtTask)
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


createTaskAtBottom :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => UserId -> NewTask -> SqlPersistT m (Entity Task)
createTaskAtBottom userId task = do
  time <- now
  maybeLastTask <- selectFirst [TaskUser ==. userId] [Desc TaskOrder]
  let lastOrder = maybe 0 (taskOrder . entityVal) maybeLastTask
  createTask userId time (succ lastOrder) task


findTaskByExtTask :: (PersistUnique (SqlPersistT m), PersistQuery (SqlPersistT m), GetExtTask extTask) => UserId -> extTask -> SqlPersistT m (Maybe (Entity Task))
findTaskByExtTask userId extTask = do
  mExtTask <- getExtTask userId extTask
  case mExtTask of
    Just (Entity extTaskId _) -> selectFirst [TaskUser ==. userId, TaskExtTask ==. Just extTaskId] []
    Nothing -> return Nothing


completeTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => TimeZone -> Entity Task -> SqlPersistT m (Maybe (Entity Task), Maybe (Entity Task))
completeTask tz taskEntity = do
    time <- now
    maybeNextTime <- recurTask tz time taskEntity
    completed <- updateReturningNew (entityKey taskEntity) [TaskDoneAt =. Just time]
    return (completed, maybeNextTime)

restartTask :: (Functor m, PersistQuery (SqlPersistT m)) => TaskId -> SqlPersistT m (Maybe (Entity Task))
restartTask = flip updateReturningNew [TaskDoneAt =. Nothing]


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


duplicateTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => TimeZone -> UTCTime -> Entity Task -> SqlPersistT m TaskId
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
    copyFirstEstimate :: (Functor m, PersistQuery (SqlPersistT m)) => TaskId -> TaskId -> SqlPersistT m [EstimateId]
    copyFirstEstimate taskId newTaskId = do
      firstEstimate <- take 1 <$> taskEstimates taskId
      mapM (copyEstimate newTaskId . entityVal) firstEstimate
    copyEstimate :: PersistQuery (SqlPersistT m) => TaskId -> Estimate -> SqlPersistT m EstimateId
    copyEstimate newTaskId (Estimate _ pomos) = insert $ Estimate newTaskId pomos


deleteTask :: PersistQuery (SqlPersistT m) => Entity Task -> SqlPersistT m ()
deleteTask (Entity taskId task) = do
  deleteCascade taskId
  maybeM () delete $ taskExtTask task


recurTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => TimeZone -> UTCTime -> Entity Task -> SqlPersistT m (Maybe (Entity Task))
recurTask tz time taskEntity = do
    let recurrence = scheduleRecurrence $ taskSchedule (entityVal taskEntity)
    maybe (return Nothing) (applyRecurrence >=> return . Just) recurrence
  where
    applyRecurrence :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => NominalDiffTime -> SqlPersistT m (Entity Task)
    applyRecurrence recurrence = do
      newTaskId <- duplicateTask tz time taskEntity
      mNewRecurrence <- postponeTask recurrence newTaskId
      maybeM (error "should never happen") return mNewRecurrence


data TaskEdit = TaskTitleEdit { taskTitleAfter :: Text }
              | TaskOrderEdit { taskOrderDelta :: Int }
              | TaskSyncEdit { taskSyncTask :: NewTask }
              | TaskPomosEdit { taskPomosAdded :: Int }
              deriving (Show)


instance FromJSON TaskEdit where
  parseJSON (Object o) = do
    editType <- o .: "editType"
    case editType of
      "title" -> TaskTitleEdit <$> o .: "title"
      "order" -> TaskOrderEdit <$> o .: "delta"
      "pomos" -> TaskPomosEdit <$> o .: "pomos"
      _ -> fail $ "don't understand task edit type \"" ++ editType ++ "\""
  parseJSON v = fail $ "can't parse task edit: " ++ show v


updateTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => TimeZone -> TaskEdit -> TaskId -> SqlPersistT m (Bool, Maybe (Entity Task))
updateTask tz edit taskId = do
    mtask <- get taskId
    case mtask of
      Just task -> do
        updated <- updateTask' edit task
        (updated,) . fmap (Entity taskId) <$> if updated then get taskId else return $ Just task
      Nothing -> return (False, Nothing)
  where
    updateTask' (TaskTitleEdit title) task
      | taskTitle task /= title = update taskId [TaskTitle =. title] >> return True
      | otherwise               = return False
    updateTask' (TaskOrderEdit delta) task = reorderTaskN delta tz (taskId, task)
    updateTask' (TaskSyncEdit (NewTask newTitle _ _)) task = updateTask' (TaskTitleEdit newTitle) task
    updateTask' (TaskPomosEdit 0) _ = return False
    updateTask' (TaskPomosEdit pomos) _ = update taskId [TaskPomos +=. pomos] >> return True


data Direction = Up | Down deriving (Show, Enum, Bounded)

nextTask :: (MonadIO m, PersistQuery (SqlPersistT m)) => Direction -> TimeZone -> Task -> SqlPersistT m (Maybe (Entity Task))
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

reorderTask :: (MonadIO m, PersistQuery (SqlPersistT m)) => Direction -> TimeZone -> (TaskId, Task) -> SqlPersistT m (TaskId, Task)
reorderTask direction tz (taskId, task) = do
  maybeNext <- nextTask direction tz task
  case maybeNext of
    Nothing -> return (taskId, task)
    Just (Entity nextId next) -> do
      update taskId [TaskOrder =. minBound] -- temporary value
      update nextId [TaskOrder =. taskOrder task]
      update taskId [TaskOrder =. taskOrder next]
      return (taskId, task { taskOrder = taskOrder next })


reorderTaskN :: (MonadIO m, PersistQuery (SqlPersistT m)) => Int -> TimeZone -> (TaskId, Task) -> SqlPersistT m Bool
reorderTaskN delta tz task
  | delta > 0 = foldTimesM delta (reorder Down) task >> return True
  | delta < 0 = foldTimesM (-delta) (reorder Up) task >> return True
  | otherwise = return False
  where
    reorder direction = reorderTask direction tz


taskTodo :: TimeZone -> UTCTime -> Task -> Bool
taskTodo tz moment task = taskActive task && taskScheduledForDay tz task <= today
  where today = utcToLocalDay tz moment

taskPostponed :: TimeZone -> UTCTime -> Task -> Bool
taskPostponed tz moment = not . taskTodo tz moment

taskScheduledForDay :: TimeZone -> Task -> Day
taskScheduledForDay tz = utcToLocalDay tz . taskScheduledFor


taskEstimates :: PersistQuery (SqlPersistT m) => TaskId -> SqlPersistT m [Entity Estimate]
taskEstimates taskId = selectList [EstimateTask ==. taskId] []

estimateOptions :: [Int]
estimateOptions = 0 : [2 ^ x | x <- [0 .. 3] :: [Int]]


postponeTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => NominalDiffTime -> TaskId -> SqlPersistT m (Maybe (Entity Task))
postponeTask postponement taskId = do
  postponed <- hence postponement
  updateReturningNew taskId [TaskScheduledFor =. postponed]


unpostponeTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => TaskId -> SqlPersistT m (Maybe (Entity Task))
unpostponeTask taskId = do
  time <- now
  updateReturningNew taskId [TaskScheduledFor =. time]


pauseTask :: (Functor m, PersistQuery (SqlPersistT m)) => TaskId -> SqlPersistT m (Maybe (Entity Task))
pauseTask taskId = updateReturningNew taskId [TaskActive =. False]

unpauseTask :: (Functor m, MonadIO m, PersistQuery (SqlPersistT m)) => TaskId -> SqlPersistT m (Maybe (Entity Task))
unpauseTask taskId = update taskId [TaskActive =. True] >> unpostponeTask taskId


taskGetExtTask :: PersistStore (SqlPersistT m) => Task -> SqlPersistT m (Maybe ExtTask)
taskGetExtTask task = maybeM Nothing get $ taskExtTask task


allExtTasksForSource :: PersistQuery (SqlPersistT m) => UserId -> ExternalSourceName -> SqlPersistT m [Entity ExtTask]
allExtTasksForSource userId source = selectList [
    ExtTaskUser ==. userId
  , ExtTaskExtSourceName ==. source
  ] []


featureSettings :: Maybe User -> Flags Feature
featureSettings = defaultMissing . maybe defaultFeatureSettings userFeatures


defaultFeatureSettings :: Flags Feature
defaultFeatureSettings = mempty


toggleUserFeature :: PersistQuery (SqlPersistT m) => Feature -> Entity User -> SqlPersistT m ()
toggleUserFeature feature (Entity userId user) = do
  let features = toggleFlag feature $ userFeatures user
  update userId [UserFeatures =. features]
