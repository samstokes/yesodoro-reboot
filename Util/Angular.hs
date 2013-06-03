module Util.Angular
  ( requireAuthIdPreventingXsrf
  , setXsrfCookie
  , validXsrfHeader
  ) where


import Prelude hiding (mapM_)
import Data.Foldable (mapM_)

import Control.Monad (unless)
import Data.Default (def)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request(..))
import Web.Cookie (SetCookie(..))
import Yesod.Auth
import Yesod.Core
import Yesod.Form.Types (FormMessage(MsgCsrfWarning))


setXsrfCookie :: GHandler s m ()
setXsrfCookie = do
    request <- getRequest
    let token = reqToken request
        secure = isSecure $ reqWaiRequest request
    mapM_ (setCookie . xsrfCookie secure) token
  where xsrfCookie secure token = def {
      setCookieName = "XSRF-TOKEN"
    , setCookieValue = encodeUtf8 token
    , setCookieExpires = Nothing -- session cookie
    , setCookieSecure = secure
    , setCookieHttpOnly = False -- whole point is for Angular to read it
    }


validXsrfHeader :: GHandler s m Bool
validXsrfHeader = do
  token <- fmap reqToken getRequest
  mSuppliedToken <- fmap (fmap decodeUtf8 . lookup "X-XSRF-Token" . requestHeaders) waiRequest
  return $ token == mSuppliedToken


requireAuthIdPreventingXsrf :: YesodAuth m => GHandler s m (AuthId m)
requireAuthIdPreventingXsrf = do
  valid <- validXsrfHeader
  unless valid $ do
    m <- getYesod
    langs <- languages
    sendResponseStatus HTTP.badRequest400 $ RepPlain $ toContent $ renderMessage m langs MsgCsrfWarning
  requireAuthId
