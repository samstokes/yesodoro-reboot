-- Based on yesod-core-1.0.1.3/Yesod/Internal/{Core,Session}.hs
-- modified to set the 'secure' cookie parameter for prod
--
-- This whole file may become obsolete when we upgrade yesod as newer versions
-- appear to have a customizeSessionCookie function

module Util.SessionBackend (
    secureClientSessionBackend
) where


import Prelude

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.Serialize
import Data.Time
import qualified Network.Wai as W
import qualified Web.ClientSession as CS
import Web.Cookie
import Yesod hiding (get)


-- as clientSessionBackend, but sets the 'secure' cookie parameter if the
-- approot is https
secureClientSessionBackend :: Yesod master
                     => CS.Key  -- ^ The encryption key
                     -> Int -- ^ Inactive session valitity in minutes
                     -> SessionBackend master
secureClientSessionBackend key timeout = SessionBackend
    { sbLoadSession = loadSecureClientSession key timeout "_SESSION"
    }

loadSecureClientSession :: Yesod master
                  => CS.Key
                  -> Int -- ^ timeout
                  -> S8.ByteString -- ^ session name
                  -> master
                  -> W.Request
                  -> UTCTime
                  -> IO (BackendSession, SaveSession)
loadSecureClientSession key timeout sessionName master req now = return (sess, save)
  where
    sess = fromMaybe [] $ do
      raw <- lookup "Cookie" $ W.requestHeaders req
      val <- lookup sessionName $ parseCookies raw
      let host = "" -- fixme, properly lock sessions to client address
      decodeClientSession key now host val
    save sess' now' = do
      -- We should never cache the IV!  Be careful!
      iv <- liftIO CS.randomIV
      return [AddCookie def
          { setCookieName = sessionName
          , setCookieValue = sessionVal iv
          , setCookiePath = Just (cookiePath master)
          , setCookieExpires = Just expires
          , setCookieSecure = secure
          , setCookieDomain = cookieDomain master
          , setCookieHttpOnly = True
          }]
        where
          host = "" -- fixme, properly lock sessions to client address
          expires = fromIntegral (timeout * 60) `addUTCTime` now'
          secure = "https://" `isPrefixOf` resolvedApproot
          sessionVal iv = encodeClientSession key iv expires host sess'
          resolvedApproot = resolveApproot master req


-- Copy-pasted from Yesod.Internal.Core because that's a hidden module :(
type ResolvedApproot = Text

resolveApproot :: Yesod master => master -> W.Request -> ResolvedApproot
resolveApproot master req =
    case approot of
        ApprootRelative -> ""
        ApprootStatic t -> t
        ApprootMaster f -> f master
        ApprootRequest f -> f master req



-- Copy-pasted from Yesod.Internal.Session because that's a hidden module :(

type SaveSession = BackendSession    -- ^ The session contents after running the handler
                -> UTCTime           -- ^ current time
                -> IO [Header]

data SessionCookie = SessionCookie UTCTime ByteString [(Text, ByteString)]
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = putTime a >> put b >> put (map (first unpack) c)
    get = do
        a <- getTime
        b <- get
        c <- map (first pack) <$> get
        return $ SessionCookie a b c

putTime :: Putter UTCTime
putTime t@(UTCTime d _) = do
    put $ toModifiedJulianDay d
    let ndt = diffUTCTime t $ UTCTime d 0
    put $ toRational ndt

getTime :: Get UTCTime
getTime = do
    d <- get
    ndt <- get
    return $ fromRational ndt `addUTCTime` UTCTime (ModifiedJulianDay d) 0

encodeClientSession :: CS.Key
                    -> CS.IV
                    -> UTCTime -- ^ expire time
                    -> ByteString -- ^ remote host
                    -> [(Text, ByteString)] -- ^ session
                    -> ByteString -- ^ cookie value
encodeClientSession key iv expire rhost session' =
    CS.encrypt key iv $ encode $ SessionCookie expire rhost session'

decodeClientSession :: CS.Key
                    -> UTCTime -- ^ current time
                    -> ByteString -- ^ remote host field
                    -> ByteString -- ^ cookie value
                    -> Maybe [(Text, ByteString)]
decodeClientSession key now rhost encrypted = do
    decrypted <- CS.decrypt key encrypted
    SessionCookie expire rhost' session' <-
        either (const Nothing) Just $ decode decrypted
    guard $ expire > now
    guard $ rhost' == rhost
    return session'
