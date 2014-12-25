module Util.Angular
  ( requireNgAuthId
  , requireNgAuth
  , setXsrfCookie
  , validXsrfHeader
  ) where


import Prelude hiding (mapM_)
import Data.Foldable (mapM_)

import Control.Monad (unless)
import Data.Aeson
import Data.Default (def)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request(..))
import Web.Cookie (SetCookie(..))
import Database.Persist
import Yesod.Auth
import Yesod.Core
import Yesod.Form.Types (FormMessage(MsgCsrfWarning))
import Yesod.Json (jsonToRepJson)
import Yesod.Persist

import Util (passthru)


setXsrfCookie :: MonadHandler m => m ()
setXsrfCookie = do
    request <- getRequest
    let token = reqToken request
        -- secure = isSecure $ reqWaiRequest request
#if DEVELOPMENT
        secure = False
#else
        secure = True -- Keter terminates SSL so "isSecure request" is False
#endif
    mapM_ (setCookie . xsrfCookie secure) token
  where xsrfCookie secure token = def {
      setCookieName = "XSRF-TOKEN"
    , setCookieValue = encodeUtf8 token
    , setCookieExpires = Nothing -- session cookie
    , setCookieSecure = secure
    , setCookieHttpOnly = False -- whole point is for Angular to read it
    }


validXsrfHeader :: MonadHandler m => m Bool
validXsrfHeader = do
  token <- fmap reqToken getRequest
  mSuppliedToken <- fmap (fmap decodeUtf8 . lookup "X-XSRF-Token" . requestHeaders) waiRequest
  return $ token == mSuppliedToken


validateXsrfHeader :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => m ()
validateXsrfHeader = do
  valid <- validXsrfHeader
  unless valid $ do
    m <- getYesod
    langs <- languages
    sendResponseStatus HTTP.badRequest400 $ RepPlain $ toContent $ renderMessage m langs MsgCsrfWarning


respondUnauthorized :: MonadHandler m => m b
respondUnauthorized = do
  errorJson <- jsonToRepJson $ object ["message" .= ("Please log in." :: String)]
  sendResponseStatus HTTP.unauthorized401 errorJson


requireNgAuthId :: YesodAuth site => HandlerT site IO (AuthId site)
requireNgAuthId = maybeAuthId >>= maybe respondUnauthorized return >>= passthru validateXsrfHeader


requireNgAuth ::
  ( YesodPersist site
  , YesodAuth site
  , PersistEntity auth
  , Typeable auth
  , PersistStore (YesodPersistBackend site (HandlerT site IO))
  , AuthId site ~ Key auth
  , PersistMonadBackend (YesodPersistBackend site (HandlerT site IO)) ~ PersistEntityBackend auth
  ) => HandlerT site IO (Entity auth)
requireNgAuth = maybeAuth >>= maybe respondUnauthorized return >>= passthru validateXsrfHeader
