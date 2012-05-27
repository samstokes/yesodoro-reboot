module Util where

import Prelude
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (groupBy)
import Data.Time
import Text.Blaze (ToMarkup(..))


now :: MonadIO m => m UTCTime
now = liftIO getCurrentTime

utcToLocalDay :: TimeZone -> UTCTime -> Day
utcToLocalDay tz = localDay . utcToLocalTime tz

tomorrow :: UTCTime -> UTCTime
tomorrow = addUTCTime oneDay
  where oneDay = 24 * 60 * 60

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
