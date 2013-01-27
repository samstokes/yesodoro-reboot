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
      $(widgetFile def "homepage")
  getHomeR' (Just _) = redirect TasksR
