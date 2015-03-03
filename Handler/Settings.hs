{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Settings where

import Data.Aeson ((.:?))
import qualified Data.Aeson as J
import qualified Data.Text as Text

import Import

import Util
import Util.Angular

data UserSettingsUpdate = UserSettingsUpdate {
    usTimezone :: TimezoneSpecifier
  } deriving (Show)

instance FromJSON UserSettingsUpdate where
  parseJSON (J.Object o) = UserSettingsUpdate <$> o .: "timezone"
  parseJSON j = fail $ "can't parse user settings update: " ++ show j

data TimezoneSpecifier = TimezoneByOffset Int | Unknown Text
  deriving (Show)

instance FromJSON TimezoneSpecifier where
  parseJSON (J.Object o) = do
    mOffset <- o .:? "offset"
    return $ case mOffset of
      Just offset -> TimezoneByOffset offset
      Nothing -> Unknown $ textShow o
  parseJSON (J.String s) = return $ Unknown s
  parseJSON json = return $ Unknown $ textShow json

specifiedTimezone :: TimezoneSpecifier -> TimeZone
specifiedTimezone (TimezoneByOffset offset) = minutesToTimeZone offset
specifiedTimezone (Unknown dat) = error $ "Unknown timezone specifier: " ++ Text.unpack dat


putHeyR :: Handler J.Value
putHeyR = do
  (Entity uid user) <- requireNgAuth
  settings <- requireJsonBody
  let tz = specifiedTimezone $ usTimezone settings
  case userTimeZone user of
    Nothing -> do
      runDB $ update uid [UserTimeZone =. Just tz]
      return $ J.object ["updated" .= show tz]
    -- TODO this is a shitty response because this is general?
    Just tz' | timeZoneMinutes tz == timeZoneMinutes tz' -> return $ object ["updated" .= False]
    Just tz' -> do
      $logInfo $ Text.concat [
        "User timezone changed from ",
        textShow tz', " to ", textShow tz,
        "!  We should really do something about that."]
      return $ object ["updated" .= False]

postToggleFeatureR :: Feature -> Handler Html
postToggleFeatureR feature = do
  user <- requireAuth
  runDB $ toggleUserFeature feature user
  redirect TasksR
