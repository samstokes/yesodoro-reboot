module Util.Http
  ( withEtag, withDigestEtag
  ) where


import Prelude

import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (requestHeaders)
import Yesod.Core (MonadHandler, RepPlain(..), sendResponseStatus, addHeader, toContent, waiRequest)


withDigestEtag :: MonadHandler m => m BL.ByteString -> m a -> m a
withDigestEtag getEtagMatter = withEtag (fmap digestEtag getEtagMatter)
  -- hackily get a string out of the MD5Digest via 'show'
  where digestEtag = T.pack . show . md5


withEtag :: MonadHandler m => m Text -> m a -> m a
withEtag getEtag handler = do
  etag <- getEtag
  request <- waiRequest
  let headers = requestHeaders request
  let ifNoneMatch = lookup "If-None-Match" headers
  if maybe False (== encodeUtf8 etag) ifNoneMatch
  then
    sendResponseStatus HTTP.notModified304 $ RepPlain $ toContent T.empty
  else
    setHeader "ETag" (etag) >> handler
