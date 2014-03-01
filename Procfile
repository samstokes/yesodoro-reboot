# Free deployment to Heroku.
#
#   !! Warning: You must use a 64 bit machine to compile !!
#
#   This could mean using a virtual machine. Give your VM as much memory as you can to speed up linking.
#
# Basic Yesod setup:
#
# * Move this file out of the deploy directory and into your root directory
#
#     mv deploy/Procfile ./
#
# * Create an empty package.json
#     echo '{ "name": "yesodoro-reboot", "version": "0.0.1", "dependencies": {} }' >> package.json
#
# Postgresql Yesod setup:
#
# * add a dependency on the "heroku" package in your cabal file
#
# * add code in Application.hs to use the heroku package and load the connection parameters.
#   The below works for Postgresql.
#
#   #ifndef DEVELOPMENT
#   import qualified Web.Heroku
#   #endif
#
#
#   makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
#   makeApplication conf logger = do
#       manager <- newManager def
#       s <- staticSite
#       hconfig <- loadHerokuConfig
#       dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
#                 (Database.Persist.Store.loadConfig . combineMappings hconfig) >>=
#                 Database.Persist.Store.applyEnv
#       p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
#       Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
#    let foundation = App conf setLogger s p manager dbconf
#    app <- toWaiAppPlain foundation
#    return $ logWare app
#  where
##ifdef DEVELOPMENT
#    logWare = logCallbackDev (logBS setLogger)
#    setLogger = logger
##else
#    setLogger = toProduction logger -- by default the logger is set for development
#    logWare = logCallback (logBS setLogger)
##endif
#
#   #ifndef DEVELOPMENT
#   canonicalizeKey :: (Text, val) -> (Text, val)
#   canonicalizeKey ("dbname", val) = ("database", val)
#   canonicalizeKey pair = pair
#
#   toMapping :: [(Text, Text)] -> AT.Value
#   toMapping xs = AT.Object $ M.fromList $ map (\(key, val) -> (key, AT.String val)) xs
#   #endif
#
#   combineMappings :: AT.Value -> AT.Value -> AT.Value
#   combineMappings (AT.Object m1) (AT.Object m2) = AT.Object $ m1 `M.union` m2
#   combineMappings _ _ = error "Data.Object is not a Mapping."
#
#   loadHerokuConfig :: IO AT.Value
#   loadHerokuConfig = do
#   #ifdef DEVELOPMENT
#       return $ AT.Object M.empty
#   #else
#       Web.Heroku.dbConnParams >>= return . toMapping . map canonicalizeKey
#   #endif



# Heroku setup:
# Find the Heroku guide. Roughly:
#
# * sign up for a heroku account and register your ssh key
# * create a new application on the *cedar* stack
#
# * make your Yesod project the git repository for that application
# * create a deploy branch
#
#     git checkout -b deploy
#
# Repeat these steps to deploy:
# * add your web executable binary (referenced below) to the git repository
#
#     git checkout deploy
#     git add ./dist/build/yesodoro-reboot/yesodoro-reboot
#     git commit -m deploy
#
# * push to Heroku
#
#     git push heroku deploy:master


# Heroku configuration that runs your app
web: ./script/run-heroku.sh $YESOD_ENV -p $PORT
