module Settings.Development where

import Prelude

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development


ifDev :: a -> a -> a
ifDev whenDev whenProd = if development then whenDev else whenProd

eitherDev :: a -> b -> Either a b
eitherDev whenDev whenProd = ifDev (Left whenDev) (Right whenProd)
