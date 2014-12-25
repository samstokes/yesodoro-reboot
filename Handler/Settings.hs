module Handler.Settings where

import Import

postToggleFeatureR :: Feature -> Handler Html
postToggleFeatureR feature = do
  user <- requireAuth
  runDB $ toggleUserFeature feature user
  redirect TasksR
