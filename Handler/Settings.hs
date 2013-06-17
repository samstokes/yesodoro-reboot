module Handler.Settings where

import Import

postToggleFeatureR :: Feature -> Handler RepHtml
postToggleFeatureR feature = do
  user <- requireAuth
  runDB $ toggleUserFeature feature user
  redirect TasksR
