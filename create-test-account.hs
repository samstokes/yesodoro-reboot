{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (MonadLogger, logInfoN, runStderrLoggingT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTimeZone)
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as Sql
import Safe (readMay)
import System.Environment (getArgs)
import qualified Yesod.Auth.Email as AuthEmail
import qualified Yesod.Default.Config as Y

import Application (makeDatabasePool)
import Model
import Settings (parseExtra)
import Types

main :: IO ()
main = do
    (env, email, plainPassword) <- parseArgs
    conf <- Y.loadConfig (Y.configSettings env) { Y.csParseExtra = parseExtra }
    (dbconf, pool) <- makeDatabasePool conf
    saltedPassword <- AuthEmail.saltPass plainPassword
    run $ DB.runPool dbconf (createTestUser email saltedPassword) pool
  where
    run = runResourceT . runStderrLoggingT

parseArgs :: IO (Y.DefaultEnv, Text, Text)
parseArgs = parseArgs' <$> getArgs
  where parseArgs' [readMay -> Just env, email, password] = (env, Text.pack email, Text.pack password)
        parseArgs' [badEnv, _, _] = error $ "Unrecognised environment: " ++ badEnv
        parseArgs' _ = error "Args: environment test-email test-password"

createTestUser :: (MonadLogger m, DB.PersistStore m, Sql.SqlBackend ~ DB.PersistMonadBackend m) => Text -> Text -> m ()
createTestUser email saltedPassword = do
  tz <- liftIO getCurrentTimeZone
  uid <- DB.insert $ User email (Just saltedPassword) tz noFlags
  eid <- DB.insert $ Email email (Just uid) Nothing
  logInfoN $ Text.intercalate " " [
      "Created user"
    , showKey uid
    , "for email"
    , email
    , Text.concat ["(email id ", showKey eid, ")"]
    ]
  return ()

showKey :: DB.KeyBackend b e -> Text
showKey (DB.unKey -> DB.PersistInt64 n) = Text.pack $ show n
showKey key = Text.pack $ "unexpected key type: " ++ show key
