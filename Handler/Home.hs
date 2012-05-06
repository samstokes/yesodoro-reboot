{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler RepHtml
getHomeR = maybeAuthId >>= getHomeR' where
  getHomeR' :: Maybe UserId -> Handler RepHtml
  getHomeR' Nothing = defaultLayout $ do
      setTitle "yesodoro"
      $(widgetFile "homepage")
  getHomeR' (Just _) = {-redirect-} undefined
