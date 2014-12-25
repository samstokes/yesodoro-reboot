{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Util

import Prelude

import Control.Applicative (pure)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as J
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time (TimeZone)
import Safe (readMay)
import Text.Blaze (ToMarkup(..))
import Yesod (PathPiece(..), derivePersistField)


derivePersistField "TimeZone"


data Feature = FeaturePomos
             | FeaturePlans
             | FeatureNotes
             | FeatureOverdueTasks
             | FeatureNonDailySchedules
             | FeatureSettings
             | FeaturePause
             | FeatureExtTasks
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

type FlagSetting a = (a, Bool)
type Flags a = [FlagSetting a]

hasFlag :: Eq a => a -> Flags a -> Bool
hasFlag k = fromMaybe False . lookup k

noFlags :: Flags a
noFlags = []

toggleFlag :: Eq a => a -> Flags a -> Flags a
toggleFlag flag oldFlags = (flag, not currentFlag) : oldFlagsWithoutFlag where
  currentFlag = hasFlag flag oldFlags
  oldFlagsWithoutFlag = filter ((/= flag) . fst) oldFlags

defaultMissing :: (Eq a, Enum a, Bounded a) => Flags a -> Flags a
defaultMissing flags = map (, False) missing ++ flags
  where missing = filter (not . oneOf (map fst flags)) [minBound .. maxBound]


featureDescription :: Feature -> String
featureDescription FeaturePomos = "Pomodoro tracking"
featureDescription FeaturePlans = "plans"
featureDescription FeatureNotes = "task notes"
featureDescription FeatureOverdueTasks = "warning for overdue tasks"
featureDescription FeatureNonDailySchedules = "more task schedule options"
featureDescription FeatureSettings = "settings panel"
featureDescription FeaturePause = "pausing tasks"
featureDescription FeatureExtTasks = "external task links"


type FlagsVoid = Flags ()
derivePersistField "FlagsVoid"

type FlagsFeature = Flags Feature
derivePersistField "FlagsFeature"


instance PathPiece Feature where
  toPathPiece = pack . show
  fromPathPiece = readMay . unpack


data Schedule = Once | Daily | Weekly | Fortnightly
  deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Schedule"

instance ToJSON Schedule where
  toJSON = toJSON . show

instance ToMarkup Schedule where
  toMarkup = toMarkup . show

schedules :: [Schedule]
schedules = [minBound .. maxBound]

nonDaily :: Schedule -> Bool
nonDaily = oneOf [Weekly, Fortnightly]

instance FromJSON Schedule where
  parseJSON (J.String s) = case reads (unpack s) of
      [(schedule, "")] -> pure schedule
      _ -> err "not a schedule: "
    where err = fail . (++ unpack s)
  parseJSON v = fail $ "can't parse as schedule: " ++ show v


scheduleRecurrence :: Num a => Schedule -> Maybe a
scheduleRecurrence Once = Nothing
scheduleRecurrence Daily = Just $ days 1
scheduleRecurrence Weekly = Just $ days 7
scheduleRecurrence Fortnightly = Just $ days 14

scheduleLabel :: Schedule -> Maybe Text
scheduleLabel Once = Nothing
scheduleLabel Daily = Just "♳"
scheduleLabel Weekly = Just "♹"
scheduleLabel Fortnightly = Just "♹♹"


newtype ExternalIdent = ExternalIdent { unExternalIdent :: Text }
  deriving (Read, Show, FromJSON, ToJSON, ToMarkup)
derivePersistField "ExternalIdent"

newtype ExternalSourceName = ExternalSourceName { unExternalSourceName :: Text }
  deriving (Eq, Read, Show, FromJSON, ToJSON, ToMarkup, PathPiece)
derivePersistField "ExternalSourceName"


