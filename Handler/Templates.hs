module Handler.Templates where


import Import
import Util
import Util.Http
import Util.Angular

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Char as Char
import Data.Monoid ((<>))
import Text.Blaze (Markup, toMarkup)
import Text.Hamlet (hamletFile)


getTemplateOldTasksR, getTemplateTaskTrR, getTemplateNewSettingsR, getTemplateNewTasksTodayR, getTemplateNewTasksLaterR, getTemplateNewTasksDoneR, getTemplateNewTaskR :: Handler Html


getTemplateOldTasksR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
      scheduleOptions = if has FeatureNonDailySchedules
        then schedules
        else filter (not . nonDaily) schedules
    pc <- widgetToPageContent $(whamletFile "templates/tasks/tasks-old.hamlet")
    giveUrlRenderer [hamlet|^{pageBody pc}|]

getTemplateTaskTrR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
    giveUrlRenderer $(hamletFile "templates/tasks/task-tr-ng.hamlet")


getTemplateNewSettingsR = giveUrlRenderer $(hamletFile "templates/new-design/settings.hamlet")
getTemplateNewTasksTodayR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
      scheduleOptions = if has FeatureNonDailySchedules
        then schedules
        else filter (not . nonDaily) schedules
    giveUrlRenderer $(hamletFile "templates/new-design/tasks-today.hamlet")

getTemplateNewTasksLaterR = giveUrlRenderer $(hamletFile "templates/new-design/tasks-later.hamlet")
getTemplateNewTasksDoneR = giveUrlRenderer $(hamletFile "templates/new-design/tasks-done.hamlet")
getTemplateNewTaskR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
    giveUrlRenderer $(hamletFile "templates/new-design/task.hamlet")

featuresEtag :: User -> Handler B.ByteString
featuresEtag user = return . B.pack . concat $ [
    show estimateOptions
  , show (userFeatures user)
  ]


toScheduleId :: Schedule -> Markup
toScheduleId = toMarkup . ("schedule-" <>) . map Char.toLower . show
