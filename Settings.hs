{-# LANGUAGE ViewPatterns #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    , getGoogleClientCredentials
    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import System.Environment (getEnv)
import Text.Hamlet
import Data.Maybe (catMaybes)

-- | Which Persistent backend this site is using.
type PersistConfig = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                            else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraTitle :: Text
    , extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    , extraIsHeroku :: Bool
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "title"
    <*> o .:  "copyright"
    <*> o .:? "analytics"
    <*> fmap (fromMaybe False) (o .:? "is_heroku")

getGoogleClientCredentials :: DefaultEnv -> IO (Text, Text)
getGoogleClientCredentials env = do
    dev <- loadDevSecrets env
    clientId <- getSecret dev env "GOOGLE_CLIENT_ID"
    clientSecret <- getSecret dev env "GOOGLE_CLIENT_SECRET"
    return (clientId, clientSecret)

type Secrets = [(String, Text)]

getSecret :: Secrets -> DefaultEnv -> String -> IO Text
getSecret dev Development varname = return $ fromMaybe (error $ "missing " ++ varname) $ lookup varname dev
getSecret _ Testing _ = return "dummy"
getSecret _ _ varname = T.pack <$> getEnv varname

loadDevSecrets :: DefaultEnv -> IO Secrets
loadDevSecrets Development = do
    contents <- TIO.readFile "config/dev-secrets.conf"
    return $ catMaybes $ map parseLine $ T.lines contents
  where
    parseLine "" = Nothing
    parseLine (T.head -> '#') = Nothing
    parseLine line = Just $ cleanup $ T.breakOn ":" line
    cleanup (name, "") = error $ "Bad syntax: " ++ T.unpack name
    cleanup (name, value) = (T.unpack name, T.strip $ T.drop 1 value)
loadDevSecrets _ = return []
