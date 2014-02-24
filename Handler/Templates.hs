module Handler.Templates where


import Import
import Util.Angular

import Text.Hamlet (hamletFile)


getTemplateTaskTrR :: Handler RepHtml
getTemplateTaskTrR = do
  Entity _ user <- requireAuthPreventingXsrf
  let
    features = userFeatureSettings user
    has feature = hasFlag feature features
  hamletToRepHtml $(hamletFile "templates/tasks/task-tr-ng.hamlet")
