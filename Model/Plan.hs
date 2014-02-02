{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Model.Plan
    ( selectUserPlansSince
    , NewPlan(..), createPlan
    , updatePlan
    , planDone
    , planDoneDay
    ) where

import Prelude
import Yesod

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, parseJSON, (.:), ToJSON, toJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (Day, TimeZone, UTCTime)
import Database.Persist.Sql (SqlPersistT)
import Model.Types
import Model
import Util


instance ToJSON Plan where
  toJSON plan = object [
      "body" .= planBody plan
    , "created_at" .= planCreatedAt plan
    , "done_at" .= planDoneAt plan
    ]

-- instance ToJSON (Entity Plan) where
--   toJSON (Entity k p) = object ["id" .= k, "plan" .= p]

instance ToJSON (JsonSanityWrapper (Entity Plan)) where
  toJSON (JsonSanityWrapper (Entity k p)) = object ["id" .= k, "plan" .= p]


{-selectUserPlansSince :: PersistQuery SqlPersist m => UserId -> UTCTime -> [SelectOpt Plan] -> SqlPersist m [Entity Plan]-}
selectUserPlansSince userId doneSince = selectList (belongsToUser ++ doneSinceLimit)
  where
    belongsToUser = [PlanUser ==. userId]
    doneSinceLimit = [PlanDoneAt ==. Nothing] ||. [PlanDoneAt >=. Just doneSince]


data NewPlan = NewPlan { newPlanBody :: Text } deriving (Show)

instance FromJSON NewPlan where
  parseJSON (Object o) = NewPlan <$> (o .: "body")
  parseJSON v = fail $ "can't parse plan: " ++ show v

newPlan :: UserId -> UTCTime -> NewPlan -> Plan
newPlan uid createdAt (NewPlan body) = Plan {
    planUser = uid
  , planBody = body
  , planCreatedAt = createdAt
  , planDoneAt = Nothing
  }

{-createPlan :: (MonadIO m, PersistQuery SqlPersist m) => UserId -> NewPlan -> SqlPersist m (Entity Plan)-}
createPlan uid plan = do
  time <- now
  let plan' = newPlan uid time plan
  planId <- insert plan'
  return $ Entity planId plan'

planDone :: Plan -> Bool
planDone = isJust . planDoneAt

planDoneDay :: TimeZone -> Plan -> Maybe Day
planDoneDay tz = fmap (utcToLocalDay tz) . planDoneAt


{-updatePlan :: PersistQuery SqlPersist m => NewPlan -> PlanId -> SqlPersist m (Bool, Maybe Plan)-}
updatePlan editedPlan planId = do
  mplan <- get planId
  case mplan of
    Just plan -> do
      updated <- updatePlan' editedPlan plan
      (updated,) <$> if updated then get planId else return $ Just plan
    Nothing -> return (False, Nothing)
  where
    updatePlan' (NewPlan body) plan
      | planBody plan /= body = update planId [PlanBody =. body] >> return True
      | otherwise = return False
