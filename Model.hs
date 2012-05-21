module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (Day, TimeZone, UTCTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Store (PersistValue(..))
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


taskDone :: Task -> Bool
taskDone = isJust . taskDoneAt


taskTodo :: TimeZone -> UTCTime -> Task -> Bool
taskTodo tz now task = (taskActive task) && taskScheduledForDay tz task <= today
  where today = utcToLocalDay tz now

taskOverdue :: TimeZone -> UTCTime -> Task -> Bool
taskOverdue tz now task = not (taskDone task) && taskActive task && taskScheduledForDay tz task < today
  where today = utcToLocalDay tz now

taskDoneDay :: TimeZone -> Task -> Maybe Day
taskDoneDay tz = fmap (utcToLocalDay tz) . taskDoneAt

taskScheduledForDay :: TimeZone -> Task -> Day
taskScheduledForDay tz = utcToLocalDay tz . taskScheduledFor

taskState :: Task -> TaskState
taskState task = if taskDone task then "done" else "pending"
