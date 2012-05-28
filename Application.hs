{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
{-import Yesod.Logger (Logger, logBS, toProduction, flushLogger)-}
import qualified Network.Wai.Middleware.Gzip as GZ
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

#ifdef DEVELOPMENT
import qualified Data.Aeson.Types as Aeson.Types
#else
-- stuff for Heroku config parsing
import Control.Arrow (second)
import qualified Web.Heroku
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Tasks
import Handler.Plans

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ foldr ($) app middlewares
  where
    middlewares = [
        methodOverride
      , logWare
      , GZ.gzip GZ.def
      ]
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    _ <- forkIO $ forever $ do
        threadDelay $ 1000 * 1000
        {-flushLogger setLogger-}

    manager <- newManager def
    s <- staticSite
    heroku <- loadHerokuConfig
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              (Database.Persist.Store.loadConfig . combineMappings heroku) >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    return $ App conf s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }


#ifndef DEVELOPMENT
canonicalizeKey :: (Text, val) -> (Text, val)
canonicalizeKey ("dbname", val) = ("database", val)
canonicalizeKey pair = pair

toMapping :: [(Text, Text)] -> Aeson.Value
toMapping xs = Aeson.object $ map (second Aeson.String) xs
#endif

combineMappings :: Aeson.Value -> Aeson.Value -> Aeson.Value
combineMappings (Aeson.Object m1) (Aeson.Object m2) = Aeson.Object $ HashMap.union m1 m2
combineMappings _ _ = error "not a mapping!"

loadHerokuConfig :: IO Aeson.Value
#ifdef DEVELOPMENT
loadHerokuConfig = return $ Aeson.Types.emptyObject
#else
loadHerokuConfig = Web.Heroku.dbConnParams >>= return . toMapping . map canonicalizeKey
#endif
