module Handler.Templates where


import Import
import Util.Http
import Util.Angular

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Hamlet (hamletFile)


getTemplateTaskTrR :: Handler RepHtml
getTemplateTaskTrR = do
  Entity _ user <- requireNgAuth
  withDigestEtag (featuresEtag user) $ do
    let
      features = userFeatureSettings user
      has feature = hasFlag feature features
    hamletToRepHtml $(hamletFile "templates/tasks/task-tr-ng.hamlet")

featuresEtag :: User -> Handler B.ByteString
featuresEtag user = return . B.pack . concat $ [
    show estimateOptions
  , show (userFeatures user)
  ]
