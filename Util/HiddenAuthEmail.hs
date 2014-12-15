module Util.HiddenAuthEmail (
    authHiddenEmail
  ) where

import Prelude

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Core


-- Wrapper for email auth plugin intended only for use by automated tests.
-- Renders an invisible login form on the login page, and processes only
-- login (ignores registration, logout, email verification, etc).
authHiddenEmail :: YesodAuthEmail m => AuthPlugin m
authHiddenEmail = AuthPlugin "hiddenEmail" dispatch $ \tm -> do
    toWidget [lucius|
#hidden-auth-email {
  display: none;
}
|]
    [whamlet|
$newline never
<form id="hidden-auth-email" method="post" action="@{tm hiddenLoginR}?ORLY=YA+RLY">
    <input type="email" name="email">
    <input type="password" name="password">
    <button type=submit .btn .btn-success>Secret email login
|]
  where
    dispatch "POST" ["login"] = do
      params <- reqGetParams <$> lift getRequest
      let orly = isJust $ lookup "ORLY" params
      if orly then
        realDispatch "POST" ["login"]
      else
        error "ORLY?"
    dispatch _ _ = error "Move along, nothing to see here."
    realDispatch = apDispatch $ authEmail


hiddenLoginR :: AuthRoute
hiddenLoginR = PluginR "hiddenEmail" ["login"]
