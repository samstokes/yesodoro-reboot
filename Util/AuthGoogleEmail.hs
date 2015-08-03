module Util.AuthGoogleEmail (
    authBetterGoogleEmail
  ) where

import Prelude

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Control.Arrow (second)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (renderQueryText)
import Network.Mail.Mime (randomString)
import System.Random (newStdGen)
import Yesod.Auth
import Yesod.Auth.GoogleEmail2
import Yesod.Core


-- Wrapper for Yesod.Auth.GoogleEmail2 authGoogleEmail plugin to get control
-- of the generated HTML.
--
-- A lot of this copy-pasted from Yesod.Auth.GoogleEmail2... *sigh*
authBetterGoogleEmail :: YesodAuth m
                      => Text -- ^ client ID
                      -> Text -- ^ client secret
                      -> AuthPlugin m
authBetterGoogleEmail clientID clientSecret = basePlugin { apName = pid, apLogin = login }
  where
    basePlugin = authGoogleEmail clientID clientSecret

    login tm = do
        url <- getDest tm
        [whamlet|
<a href=#{url}>
  <.button-sign-in>
|]

    getDest :: MonadHandler m
            => (Route Auth -> Route (HandlerSite m))
            -> m Text
    getDest tm = do
        csrf <- getCreateCsrfToken
        render <- getUrlRender
        let qs = map (second Just)
                [ ("scope", "email")
                , ("state", csrf)
                , ("redirect_uri", render $ tm complete)
                , ("response_type", "code")
                , ("client_id", clientID)
                , ("access_type", "offline")
                ]
        return $ decodeUtf8
               $ toByteString
               $ fromByteString "https://accounts.google.com/o/oauth2/auth"
                    `mappend` renderQueryText True qs

    complete = PluginR pid ["complete"]


-- The following all copy-pasted

pid :: Text
pid = "googleemail2"

csrfKey :: Text
csrfKey = "_GOOGLE_CSRF_TOKEN"

getCsrfToken :: MonadHandler m => m (Maybe Text)
getCsrfToken = lookupSession csrfKey

getCreateCsrfToken :: MonadHandler m => m Text
getCreateCsrfToken = do
    mtoken <- getCsrfToken
    case mtoken of
        Just token -> return token
        Nothing -> do
            stdgen <- liftIO newStdGen
            let token = T.pack $ fst $ randomString 10 stdgen
            setSession csrfKey token
            return token
