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
import Yesod.Logger (Logger, logBS, toProduction, flushLogger)
import qualified Network.Wai.Middleware.Gzip as GZ
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Api
import Handler.Home
import Handler.Tasks
import Handler.Plans
import Handler.Settings

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    return $ foldr ($) app middlewares
  where
    middlewares = [
        methodOverride
      , logWare
      , GZ.gzip GZ.def
      ]
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation conf setLogger = do
    _ <- forkIO $ forever $ do
        threadDelay $ 1000 * 1000
        flushLogger setLogger

    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    return $ App conf setLogger s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
