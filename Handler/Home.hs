{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Blaze (toMarkup)
import Yesod.Auth

getHomeR :: Handler RepHtml
getHomeR = maybeAuthId >>= getHomeR' where
  getHomeR' :: Maybe UserId -> Handler RepHtml
  getHomeR' Nothing = defaultLayout $ do
      title <- lift appTitle
      setTitle $ toMarkup title
      $(widgetFile "homepage")
  getHomeR' (Just _) = redirect TasksR

getLoginPopupR :: Handler RepHtml
getLoginPopupR = do
  _ <- requireAuthId
  defaultLayout $ do
    toWidgetHead [hamlet|
      <script>window.close();
    |]
    setTitle "Please close this window."
    [whamlet|
      <h1>Please close this window.
    |]
