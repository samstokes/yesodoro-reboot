{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Blaze (toMarkup)
import Yesod.Auth

{-getHomeR :: Handler RepHtml-}
getHomeR = maybeAuthId >>= getHomeR' where
  getHomeR' :: Maybe UserId -> Handler RepHtml
  getHomeR' Nothing = do
    title <- appTitle
    defaultLayout $ do
      setTitle $ toMarkup title
      $(widgetFile "homepage")
  getHomeR' (Just _) = redirect TasksR
