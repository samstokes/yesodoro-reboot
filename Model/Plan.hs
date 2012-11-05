{-# LANGUAGE FlexibleInstances #-}

module Model.Plan
    ( selectUserPlansSince
    , NewPlan(..), createPlan
    , planDone
    , planDoneDay
    ) where

import Prelude
import Yesod

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (Day, TimeZone, UTCTime)
import Database.Persist.GenericSql (SqlPersist)
import Model
import Util


selectUserPlansSince :: PersistQuery SqlPersist m => UserId -> UTCTime -> [SelectOpt Plan] -> SqlPersist m [Entity Plan]
selectUserPlansSince userId doneSince = selectList (belongsToUser ++ doneSinceLimit)
  where
    belongsToUser = [PlanUser ==. userId]
    doneSinceLimit = [PlanDoneAt ==. Nothing] ||. [PlanDoneAt >=. Just doneSince]


data NewPlan = NewPlan { newPlanBody :: Text } deriving (Show)

newPlan :: UserId -> UTCTime -> NewPlan -> Plan
newPlan uid createdAt (NewPlan body) = Plan {
    planUser = uid
  , planBody = body
  , planCreatedAt = createdAt
  , planDoneAt = Nothing
  }

createPlan :: (MonadIO m, PersistQuery SqlPersist m) => UserId -> NewPlan -> SqlPersist m (Entity Plan)
createPlan uid plan = do
  time <- now
  let plan' = newPlan uid time plan
  planId <- insert plan'
  return $ Entity planId plan'

planDone :: Plan -> Bool
planDone = isJust . planDoneAt

planDoneDay :: TimeZone -> Plan -> Maybe Day
planDoneDay tz = fmap (utcToLocalDay tz) . planDoneAt
