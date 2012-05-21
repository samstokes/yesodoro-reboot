module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (Day, TimeZone, UTCTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.GenericSql (SqlPersist)
import Database.Persist.Store (PersistValue(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.String (IsString)
import Text.Blaze (ToMarkup, toMarkup)
import Util


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")



instance ToMarkup (Key a b) where
  toMarkup = toMarkup . unKey


instance ToMarkup PersistValue where
  toMarkup (PersistInt64 i) = toMarkup $ show i


newtype TaskState = TaskState Text
  deriving (ToMarkup, IsString)


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


taskDone :: Task -> Bool
taskDone = isJust . taskDoneAt


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
