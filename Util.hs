module Util where

import Prelude
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Time
import Text.Blaze (ToMarkup(..))


now :: MonadIO m => m UTCTime
now = liftIO getCurrentTime

utcToLocalDay :: TimeZone -> UTCTime -> Day
utcToLocalDay tz = localDay . utcToLocalTime tz

tomorrow :: UTCTime -> UTCTime
tomorrow = addUTCTime oneDay
  where oneDay = 24 * 60 * 60

localEndOfDay :: LocalTime -> LocalTime
localEndOfDay time = time { localTimeOfDay = TimeOfDay 23 59 59 }

locally :: TimeZone -> (LocalTime -> LocalTime) -> UTCTime -> UTCTime
locally tz f = localTimeToUTC tz . f . utcToLocalTime tz

endOfToday :: TimeZone -> IO UTCTime
endOfToday tz = do
  utcNow <- getCurrentTime
  return $ locally tz localEndOfDay utcNow


compareBy :: Ord a => (b -> a) -> b -> b -> Ordering
compareBy f x y = compare (f x) (f y)

eqUnder :: Eq b => (a -> b) -> a -> a -> Bool
eqUnder f a b = f a == f b

groupByEq :: Eq g => (a -> g) -> [a] -> [(g, [a])]
groupByEq f as = zip gs groups where
  groups = groupBy (eqUnder f) as
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


maybeToEither :: String -> Maybe a -> Either String a
maybeToEither msg Nothing = Left msg
maybeToEither _ (Just v) = Right v
