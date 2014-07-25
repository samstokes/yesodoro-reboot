{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Data.Aeson.Types (toJSON)
import Data.List (sort)
import qualified Data.Text as Text
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Julius (renderJavascript)
import Yesod.Auth

import Handler.Client
import Import
import Util


getHomeR :: Handler RepHtml
getHomeR = maybeAuthId >>= getHomeR' where
  getHomeR' :: Maybe UserId -> Handler RepHtml
  getHomeR' Nothing = defaultLayout $ do
      title <- lift appTitle
      setTitle $ toMarkup title
      $(widgetFile "homepage")
  getHomeR' (Just _) = redirect TasksR

getNewAppR :: Handler RepHtml
getNewAppR = do
  render <- getUrlRenderParams
  let declareStates stateProvider = renderJavascript $ statesDeclJavascript render stateProvider
  newLayout $(widgetFile "new-design")


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


getTasksR :: Handler RepHtml
getTasksR = do
  Entity userId user <- requireAuth
  mmsg <- fmap renderMarkup <$> getMessage
  let
    features = userFeatureSettings user
    has feature = hasFlag feature features
    toggleableFeatures = sort $ filter ((/= FeatureSettings) . fst) features
    featureButtonLabel (feature, False) = Text.pack $ "Enable " ++ featureDescription feature
    featureButtonLabel (feature, True) = Text.pack $ "Disable " ++ featureDescription feature
  title <- appTitle
  defaultLayout $ do
    setTitle $ toMarkup title
    addWidget $(widgetFile "tasks") where


oneButton :: Text -> Text -> Route App -> Widget
oneButton classes label route = [whamlet|
  <form method=POST action=@{route}>
    <button .#{classes}>#{label}
|]
