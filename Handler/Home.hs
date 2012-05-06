{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")
