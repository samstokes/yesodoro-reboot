{-# OPTIONS_GHC -fno-warn-orphans #-}
#ifndef DEVELOPMENT
{-# LANGUAGE ViewPatterns #-} -- for Heroku config parsing
#endif

module Application
    ( makeApplication
    , getApplicationDev
    , makeDatabasePool
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import qualified Network.Wai.Middleware.Gzip as GZ
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import Network.Wai.Middleware.MethodOverride (methodOverride)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (ConnectionPool, runMigration)
import Network.HTTP.Client.Conduit (newManager)
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Yesod.Core.Types (loggerSet, Logger (Logger))

#ifndef DEVELOPMENT
-- stuff for Heroku config parsing
import qualified Web.Heroku
import Data.Text.Read (decimal)
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Api
import Handler.Home
import Handler.Tasks
import Handler.Notes
import Handler.Plans
import Handler.Settings
import Handler.Templates

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

instance (YesodDispatch site) => YesodSubDispatch Client (HandlerT site m) where
  yesodSubDispatch = error "Dummy endpoint"

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ getLogger foundation
        }

    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (getLogger foundation)
    return (foldr ($) app [
        methodOverride
      , logWare
      , GZ.gzip GZ.def
      ],
      logFunc)

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    heroku <- Just <$> loadHerokuConfig conf
    (dbconf, p) <- makeDatabasePool (appEnv conf) heroku

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf logger s p manager dbconf Client

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

makeDatabasePool :: DefaultEnv -> Maybe Aeson.Value
  -> IO (Settings.PersistConfig, ConnectionPool)
makeDatabasePool env mHeroku = do
  dbconf <- withYamlEnvironment "config/postgresql.yml" env
            (Database.Persist.loadConfig . maybe id combineMappings mHeroku) >>=
            Database.Persist.applyEnv
  pool <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)
  return (dbconf, pool)

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }


#ifndef DEVELOPMENT
canonicalizeKey :: (Text, val) -> (Text, val)
canonicalizeKey ("dbname", val) = ("database", val)
canonicalizeKey pair = pair

toMapping :: [(Text, Text)] -> Aeson.Value
toMapping xs = Aeson.object $ map mungeValue xs
  where
    mungeValue :: (Text, Text) -> (Text, Aeson.Value)
    mungeValue ("port", decimal -> Right (port, "")) = ("port", Aeson.Number $ fromInteger port)
    mungeValue ("port", badPort) = error $ "Bad Heroku postgres port: " ++ show badPort
    mungeValue (key, value) = (key, Aeson.String value)
#endif

combineMappings :: Aeson.Value -> Aeson.Value -> Aeson.Value
combineMappings (Aeson.Object m1) (Aeson.Object m2) = Aeson.Object $ HashMap.union m1 m2
combineMappings _ _ = error "not a mapping!"

loadHerokuConfig :: AppConfig DefaultEnv Extra -> IO Aeson.Value
#ifdef DEVELOPMENT
loadHerokuConfig _ = return Aeson.Types.emptyObject
#else
loadHerokuConfig conf
  | extraIsHeroku (appExtra conf) = Web.Heroku.dbConnParams >>= return . toMapping . map canonicalizeKey
  | otherwise = return Aeson.Types.emptyObject
#endif
