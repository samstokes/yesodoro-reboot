{-# LANGUAGE FlexibleInstances #-}

module Model.Plan
    ( selectUserActivePlans
    , NewPlan(..), createPlan
    ) where

import Prelude
import Yesod

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.GenericSql (SqlPersist)
import Model
import Util


selectUserActivePlans :: PersistQuery SqlPersist m => UserId -> [SelectOpt Plan] -> SqlPersist m [Entity Plan]
selectUserActivePlans userId = selectList (belongsToUser ++ isActive)
  where
    belongsToUser = [PlanUser ==. userId]
    isActive = [PlanDoneAt ==. Nothing]


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
