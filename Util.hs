{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import Prelude
import Control.Arrow ((&&&))
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
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


maybeToEither :: String -> Maybe a -> Either String a
maybeToEither msg Nothing = Left msg
maybeToEither _ (Just v) = Right v


fieldListOptions :: (Show field, Enum field, Bounded field) => [(Text, field)]
fieldListOptions = map (Text.pack . show &&& id) [minBound .. maxBound]
