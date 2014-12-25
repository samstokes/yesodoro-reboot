{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import Prelude
import Control.Applicative
import Control.Arrow (Arrow, (&&&), (>>>), first, second)
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import Data.Function (on)
import Data.List (groupBy, union)
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Database.Persist (Entity(..), Key, PersistEntity, PersistEntityBackend, PersistMonadBackend, PersistQuery, Update, get, update)
import Text.Blaze (ToMarkup(..))


now :: MonadIO m => m UTCTime
now = liftIO getCurrentTime


ago, hence :: (Functor m, MonadIO m) => NominalDiffTime -> m UTCTime
ago interval = fmap (earlier interval) now
hence interval = fmap (later interval) now


earlier, later :: NominalDiffTime -> UTCTime -> UTCTime
earlier = addUTCTime . negate
later = addUTCTime


minutes, hours, days, weeks :: Num a => a -> a

minutes = (60 *)
hours = (60 *) . minutes
days = (24 *) . hours
weeks = (7 *) . days


utcToLocalDay :: TimeZone -> UTCTime -> Day
utcToLocalDay tz = localDay . utcToLocalTime tz

localEndOfDay :: LocalTime -> LocalTime
localEndOfDay time = time { localTimeOfDay = TimeOfDay 23 59 59 }

locally :: TimeZone -> (LocalTime -> LocalTime) -> UTCTime -> UTCTime
locally tz f = localTimeToUTC tz . f . utcToLocalTime tz

endOfToday :: TimeZone -> IO UTCTime
endOfToday tz = do
  utcNow <- getCurrentTime
  return $ locally tz localEndOfDay utcNow


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

groupByEq :: Eq g => (a -> g) -> [a] -> [(g, [a])]
groupByEq f as = zip gs groups where
  groups = groupBy ((==) `on` f) as
  gs = map (f . head) groups


instance ToMarkup Day where
  toMarkup = toMarkup . show

-- Mimic Ruby's nil.to_s == ''
instance ToMarkup a => ToMarkup (Maybe a) where
  toMarkup = maybe "" toMarkup


oneOf :: Eq a => [a] -> a -> Bool
oneOf = flip elem


-- N.B. this is horribly inefficient (O(n^2) in the longer list)
-- really want a nice merge algorithm or similar
unionBothValues :: (Eq k, Monoid v1, Monoid v2) => [(k, v1)] -> [(k, v2)] -> [(k, v1, v2)]
unionBothValues a1 a2 = map bothValues allKeys
  where
    bothValues k = (k, lookupCasual k a1, lookupCasual k a2)
    allKeys = map fst a1 `union` map fst a2


squishMaybe :: Monoid m => Maybe m -> m
squishMaybe Nothing = mempty
squishMaybe (Just something) = something

orElse :: Maybe a -> a -> Maybe a
ma `orElse` b = maybe (Just b) Just ma

lookupCasual :: (Eq k, Monoid v) => k -> [(k, v)] -> v
lookupCasual k l = squishMaybe $ lookup k l


overrideMethodR :: method -> route -> (route, [(Text, method)])
overrideMethodR method route = (route, [("_method", method)])


foldTimesM :: Monad m => Int -> (a -> m a) -> a -> m a
foldTimesM n f initial = foldM f' initial (replicate n ())
  where
    f' intermediate _ = f intermediate


untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
untilM predicate f a = unless (predicate a) $ do
  a' <- f a
  untilM predicate f a'


passthru :: Monad m => m b -> a -> m a
passthru m v = m >> return v


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just v) = Right v

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft _ (Right a) = Right a
mapLeft f (Left e) = Left $ f e


paras :: Text -> [Text]
paras = map Text.unlines . splitBy Text.null . Text.split (== '\n')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = foldr addUnlessP []
    where
    addUnlessP item groups | p item = [] : groups
    addUnlessP item [] = [[item]]
    addUnlessP item (group : groups) = (item : group) : groups


fieldListOptions :: (Show field, Enum field, Bounded field) => [(Text, field)]
fieldListOptions = map (Text.pack . show &&& id) [minBound .. maxBound]


jsBool :: Bool -> String
jsBool True = "true"
jsBool False = "false"


both :: Arrow a => a b c -> a (b, b) (c, c)
both f = first f >>> second f


maybeM :: Monad m => b -> (a -> m b) -> Maybe a -> m b
maybeM = maybe . return


toMaybeT :: Monad m => Maybe a -> MaybeT m a
toMaybeT = MaybeT . return


toErrorT :: (Error e, Monad m) => Either e a -> ErrorT e m a
toErrorT = ErrorT . return


type BasicAuthCredentials = (Text, Text)
parseAuthorizationHeader :: ByteString -> Either String BasicAuthCredentials
parseAuthorizationHeader auth = case B.splitAt (B.length "Basic ") auth of
    ("Basic ", b64creds) -> do
      decoded <- B64.decode b64creds
      case B.split colon decoded of
        [username, password] -> Right $ both (Text.pack . B8.unpack) (username, password)
        _ -> Left "malformed credentials!"
    _ -> Left "malformed Authorization header!"
  where colon = B.head ":" -- WTF


updateReturningNew :: (Functor m, PersistQuery m, PersistEntity val, PersistMonadBackend m ~ PersistEntityBackend val) => Key val -> [Update val] -> m (Maybe (Entity val))
updateReturningNew key updates = update key updates >> fmap (Entity key) <$> get key
