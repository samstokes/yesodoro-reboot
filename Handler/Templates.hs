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


getTemplateOldTasksR, getTemplateTaskTrR, getTemplateNewSettingsR, getTemplateNewTasksTodayR, getTemplateNewTasksLaterR, getTemplateNewTasksDoneR, getTemplateNewTaskR :: Handler RepHtml


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
    hamletToRepHtml [hamlet|^{pageBody pc}|]

getTemplateTaskTrR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
    hamletToRepHtml $(hamletFile "templates/tasks/task-tr-ng.hamlet")


getTemplateNewSettingsR = hamletToRepHtml $(hamletFile "templates/new-design/settings.hamlet")
getTemplateNewTasksTodayR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
      scheduleOptions = if has FeatureNonDailySchedules
        then schedules
        else filter (not . nonDaily) schedules
    hamletToRepHtml $(hamletFile "templates/new-design/tasks-today.hamlet")

getTemplateNewTasksLaterR = hamletToRepHtml $(hamletFile "templates/new-design/tasks-later.hamlet")
getTemplateNewTasksDoneR = hamletToRepHtml $(hamletFile "templates/new-design/tasks-done.hamlet")
getTemplateNewTaskR = hamletToRepHtml $(hamletFile "templates/new-design/task.hamlet")

featuresEtag :: User -> Handler B.ByteString
featuresEtag user = return . B.pack . concat $ [
    show estimateOptions
  , show (userFeatures user)
  ]


toScheduleId :: Schedule -> Markup
toScheduleId = toMarkup . ("schedule-" <>) . map Char.toLower . show
