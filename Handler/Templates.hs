module Handler.Templates where


import Import
import Util
import Util.Http
import Util.Angular

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Hamlet (hamletFile)


getTemplateOldTasksR, getTemplateTaskTrR, getTemplateNewSettingsR, getTemplateNewTasksTodayR, getTemplateNewTasksLaterR, getTemplateNewTasksDoneR :: Handler RepHtml


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
getTemplateNewTasksTodayR = hamletToRepHtml $(hamletFile "templates/new-design/tasks-today.hamlet")
getTemplateNewTasksLaterR = hamletToRepHtml $(hamletFile "templates/new-design/tasks-later.hamlet")
getTemplateNewTasksDoneR = hamletToRepHtml $(hamletFile "templates/new-design/tasks-done.hamlet")

featuresEtag :: User -> Handler B.ByteString
featuresEtag user = return . B.pack . concat $ [
    show estimateOptions
  , show (userFeatures user)
  ]
