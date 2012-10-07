{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Control.Applicative ((<$>))
import Data.Text (Text)
import Data.Time (Day, TimeZone, UTCTime, NominalDiffTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.GenericSql (SqlPersist)
import Database.Persist.Store (PersistValue(..), deleteCascade)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.String (IsString)
import Text.Blaze (ToMarkup, toMarkup)
import Text.Julius (ToJavascript, toJavascript)
import Util


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade]
    $(persistFileWith lowerCaseSettings "config/models")



instance ToMarkup (Key a b) where
  toMarkup = toMarkup . unKey

instance ToJavascript (Key a b) where
  toJavascript = toJavascript . unKey


instance ToMarkup PersistValue where
  toMarkup (PersistInt64 i) = toMarkup $ show i
  toMarkup _ = undefined

instance ToJavascript PersistValue where
  toJavascript (PersistInt64 i) = toJavascript $ show i
  toJavascript _ = undefined


newtype TaskState = TaskState Text
  deriving (ToMarkup, IsString)


selectUserTasksSince :: PersistQuery SqlPersist m => UserId -> UTCTime -> [SelectOpt Task] -> SqlPersist m [Entity Task]
selectUserTasksSince userId doneSince = selectList (belongsToUser ++ doneSinceLimit)
  where
    belongsToUser = [TaskUser ==. userId]
    doneSinceLimit = [TaskDoneAt ==. Nothing] ||. [TaskDoneAt >=. Just doneSince]


selectUserRecentTasks :: (MonadIO m, PersistQuery SqlPersist m) => UserId -> [SelectOpt Task] -> SqlPersist m [Entity Task]
selectUserRecentTasks userId opts = liftIO (ago $ weeks 2) >>= flip (selectUserTasksSince userId) opts


data NewTask = NewTask { newTaskTitle :: Text } deriving (Show)

newTask :: UserId -> UTCTime -> Int -> NewTask -> Task
newTask uid scheduledFor order (NewTask title) = Task {
    taskUser = uid
  , taskTitle = title
  , taskPomos = 0
  , taskScheduledFor = scheduledFor
  , taskDoneAt = Nothing
  , taskActive = True
  , taskOrder = order
  }

createTaskAtBottom :: (MonadIO m, PersistQuery SqlPersist m) => UserId -> NewTask -> SqlPersist m TaskId
createTaskAtBottom userId task = do
  time <- now
  maybeLastTask <- selectFirst [TaskUser ==. userId] [Desc TaskOrder]
  let lastOrder = maybe 0 (taskOrder . entityVal) maybeLastTask
  insert $ newTask userId time (succ lastOrder) task


completeTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> Entity Task -> SqlPersist m ()
completeTask tz taskEntity = do
    time <- now
    update (entityKey taskEntity) [TaskDoneAt =. Just time]


cloneTask :: Int -> Task -> Task
cloneTask order task = Task {
    taskUser = taskUser task
  , taskTitle = taskTitle task
  , taskPomos = 0
  , taskScheduledFor = taskScheduledFor task
  , taskDoneAt = Nothing
  , taskActive = True
  , taskOrder = order
  }


duplicateTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> UTCTime -> Entity Task -> SqlPersist m TaskId
duplicateTask tz scheduledFor taskEntity = do
    let (taskId, task) = (entityKey taskEntity, entityVal taskEntity)

    maybeFirstTask <- selectFirst [TaskUser ==. taskUser task] [Asc TaskOrder]
    let firstOrder = maybe 0 (taskOrder . entityVal) maybeFirstTask

    let dupeTask = cloneTask (pred firstOrder) task
    dupeTaskId <- insert $ dupeTask { taskScheduledFor = scheduledFor }

    -- TODO can this loop infinitely?
    untilM ((>= taskOrder task) . taskOrder . snd) (\dupeTask' -> reorderTask Down tz dupeTask') (dupeTaskId, dupeTask)

    _ <- copyFirstEstimate taskId dupeTaskId

    return dupeTaskId
  where
    copyFirstEstimate :: PersistQuery SqlPersist m => TaskId -> TaskId -> SqlPersist m [EstimateId]
    copyFirstEstimate taskId newTaskId = do
      firstEstimate <- take 1 <$> taskEstimates taskId
      mapM (copyEstimate newTaskId . entityVal) firstEstimate
    copyEstimate :: PersistQuery SqlPersist m => TaskId -> Estimate -> SqlPersist m EstimateId
    copyEstimate newTaskId (Estimate _ pomos) = insert $ Estimate newTaskId pomos


recurTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> UTCTime -> Entity Task -> SqlPersist m TaskId
recurTask tz time taskEntity = do
  newTaskId <- duplicateTask tz time taskEntity
  postponeTask newTaskId
  return newTaskId


data TaskEdit = TaskTitleEdit { taskTitleAfter :: Text }
              | TaskOrderEdit { taskOrderDelta :: Int }
              deriving (Show)


updateTask :: (MonadIO m, PersistQuery SqlPersist m) => TimeZone -> TaskEdit -> (TaskId, Task) -> SqlPersist m Bool
updateTask _ (TaskTitleEdit title) (taskId, task)
  | taskTitle task /= title = update taskId [TaskTitle =. title] >> return True
  | otherwise               = return False
updateTask tz (TaskOrderEdit delta) task = reorderTaskN delta tz task


data Direction = Up | Down deriving (Show, Enum, Bounded)

nextTask :: (MonadIO m, PersistQuery SqlPersist m) => Direction -> TimeZone -> Task -> SqlPersist m (Maybe (Entity Task))
nextTask direction tz task = do
  endOfDay <- liftIO $ endOfToday tz
  selectFirst
    [ TaskUser ==. (taskUser task)
    , (orderConstraint direction) TaskOrder (taskOrder task)
    , TaskDoneAt ==. Nothing
    , scheduledForConstraint endOfDay TaskScheduledFor
    , TaskActive ==. True
    ] [(order direction) TaskOrder]
  where
    orderConstraint Up = (<.)
    orderConstraint Down = (>.)
    order Up = Desc
    order Down = Asc
    scheduledForConstraint endOfDay | taskScheduledFor task <= endOfDay = (<=. endOfDay)
                                    | otherwise                           = (>. endOfDay)

reorderTask :: (MonadIO m, PersistQuery SqlPersist m) => Direction -> TimeZone -> (TaskId, Task) -> SqlPersist m (TaskId, Task)
reorderTask direction tz (taskId, task) = do
  maybeNext <- nextTask direction tz task
  case maybeNext of
    Nothing -> return (taskId, task)
    Just (Entity nextId next) -> do
      update taskId [TaskOrder =. minBound] -- temporary value
      update nextId [TaskOrder =. (taskOrder task)]
      update taskId [TaskOrder =. (taskOrder next)]
      return (taskId, task { taskOrder = taskOrder next })


reorderTaskN :: (MonadIO m, PersistQuery SqlPersist m) => Int -> TimeZone -> (TaskId, Task) -> SqlPersist m Bool
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
taskTodo tz moment task = (taskActive task) && taskScheduledForDay tz task <= today
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

taskDonenessActionName :: Task -> Text
taskDonenessActionName task | taskDone task = "☹"
                            | otherwise     = "☺"

taskEstimates :: PersistQuery SqlPersist m => TaskId -> SqlPersist m [Entity Estimate]
taskEstimates taskId = selectList [EstimateTask ==. taskId] []

estimateOptions :: [Int]
estimateOptions = 0 : [2 ^ x | x <- [0 .. 3] :: [Int]]


postponeTask :: (MonadIO m, PersistQuery SqlPersist m) => NominalDiffTime -> TaskId -> SqlPersist m ()
postponeTask postponement taskId = do
  postponed <- hence postponement
  update taskId [TaskScheduledFor =. postponed]

unpostponeTask :: (MonadIO m, PersistQuery SqlPersist m) => TaskId -> SqlPersist m ()
unpostponeTask taskId = do
  time <- now
  update taskId [TaskScheduledFor =. time]


pauseTask :: PersistQuery SqlPersist m => TaskId -> SqlPersist m ()
pauseTask taskId = update taskId [TaskActive =. False]

unpauseTask :: (MonadIO m, PersistQuery SqlPersist m) => TaskId -> SqlPersist m ()
unpauseTask taskId = update taskId [TaskActive =. True] >> unpostponeTask taskId
