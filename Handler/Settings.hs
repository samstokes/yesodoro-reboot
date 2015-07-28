module Handler.Settings where

import Data.Aeson ((.:?))
import qualified Data.Aeson as J
import qualified Data.Text as Text
import Text.Shakespeare.Text (st)

import Import

import Util
import Util.Angular

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


putTimezoneR :: Handler J.Value
putTimezoneR = do
  Entity uid user <- requireNgAuth
  tz <- specifiedTimezone <$> requireJsonBody
  let logUid = show $ unKey uid
  case userTimeZone user of
    Nothing -> do
      runDB $ update uid [UserTimeZone =. Just tz]
      $logInfo [st|Set user #{logUid} timezone to #{show tz}|]
      return $ object ["updated" .= True, "timezone" .= show tz]
    Just tz' | timeZoneMinutes tz == timeZoneMinutes tz' -> do
      $logDebug [st|User #{logUid} timezone unchanged (#{show tz'} == #{show tz})|]
      return $ object ["updated" .= False, "timezone" .= show tz']
    Just tz' -> do
      runDB $ update uid [UserTimeZone =. Just tz]
      $logInfo [st|Updated user #{logUid} timezone from #{show tz'} to #{show tz}|]
      return $ object ["updated" .= True, "timezone" .= show tz]

postToggleFeatureR :: Feature -> Handler Html
postToggleFeatureR feature = do
  user <- requireAuth
  runDB $ toggleUserFeature feature user
  redirect TasksR
