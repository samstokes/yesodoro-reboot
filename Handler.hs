module Handler where

import qualified Data.Text as Text

import Import
import Util


horizonFromParams :: Handler UTCTime
horizonFromParams = do
  params <- reqGetParams <$> getRequest
  let window :: Maybe Integer
      window = read . Text.unpack <$> lookup "days" params
  ago $ fromMaybe (weeks 2) (days . fromIntegral <$> window)
