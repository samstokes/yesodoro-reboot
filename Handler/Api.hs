module Handler.Api where

import Import
import Util

import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Network.Wai (requestHeaders)
import Yesod.Auth.Email (isValidPass)
import Yesod.Default.Config (AppConfig(..))

getApiTasksR :: Handler RepJson
getApiTasksR = do
  Entity userId _ <- httpBasicAuth
  horizon <- ago $ weeks 2
  tasks <- runDB $ selectUserTasksSince userId horizon [Asc TaskScheduledFor, Desc TaskDoneAt] -- must specify sorts backwards...

  jsonToRepJson tasks

postApiTasksR :: Handler RepJson
postApiTasksR = do
  Entity userId _ <- httpBasicAuth
  newTask <- parseJsonBody_ -- TODO error page is HTML, not friendly!

  existingTask <- runDB $ runMaybeT $ do
    extTask <- toMaybeT $ newTaskExt newTask
    updated <- lift $ syncExtTask userId extTask
    existingTask <- MaybeT $ findTaskByExtTask userId extTask
    return (updated, existingTask)

  case existingTask of
    Just (extUpdated, Entity taskId _) -> do
      (taskUpdated, _) <- runDB $ updateTask undefined (TaskSyncEdit newTask) taskId
      setLocation $ TaskR taskId
      jsonToRepJson $ object ["updated" .= (taskUpdated || extUpdated)]
    Nothing -> do
      taskId <- runDB $ createTaskAtBottom userId newTask
      sendResponseCreated $ TaskR taskId


getApiExtTasksForSourceR :: ExternalSourceName -> Handler RepJson
getApiExtTasksForSourceR source = do
  Entity userId _ <- httpBasicAuth
  extTasks <- runDB $ allExtTasksForSource userId source
  jsonToRepJson $ map entityVal extTasks


postApiCompleteTaskR :: TaskId -> Handler RepJson
postApiCompleteTaskR taskId = do
  Entity userId user <- httpBasicAuth
  maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
  task <- case maybeAuthedTask of
    Just task -> return task
    Nothing -> notFound
  let tz = userTimeZone user
  _ <- runDB $ completeTask tz task
  jsonToRepJson $ object ["completed" .= True]


postApiPostponeTaskR :: TaskId -> Handler RepJson
postApiPostponeTaskR taskId = do
  Entity userId _ <- httpBasicAuth
  maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
  _ <- case maybeAuthedTask of
    Just task -> return task
    Nothing -> notFound
  _ <- runDB $ postponeTask (days 1) taskId
  jsonToRepJson $ object ["postponed" .= True]


setLocation :: Route master -> GHandler sub master ()
setLocation url = do
  r <- getUrlRender
  setHeader "Location" $ r url


data AuthError = AuthCredentialsNotSupplied
               | AuthMalformedCredentials String
               | AuthCredentialsNotRecognised
               | AuthCredentialsInvalid
               | AuthErrorOther String
  deriving (Show)

instance Error AuthError where
  strMsg = AuthErrorOther


httpBasicAuth :: Handler (Entity User)
httpBasicAuth = do
    root <- (appRoot . settings) <$> getYesod
    auth <- runErrorT handleBasicAuth
    $logDebug $ T.pack $ "HTTP Basic auth: " ++ show (entityKey <$> auth)
    case auth of
      Right user -> return user

      Left AuthCredentialsNotSupplied -> do
        setHeader "WWW-Authenticate" $ T.concat ["Basic Realm=\"", root, "\""]
        permissionDenied "Authentication required"
      Left (AuthMalformedCredentials err) ->
        sendResponseStatus HTTP.badRequest400 $ RepPlain $ toContent $ T.pack err
      Left AuthCredentialsNotRecognised -> unauthorized "credentials not recognised"
      Left AuthCredentialsInvalid -> unauthorized "credentials not recognised"
      Left (AuthErrorOther err) -> do
        $logWarn $ T.pack $ "unexpected auth error: " ++ err
        unauthorized "credentials not recognised"
  where
  handleBasicAuth :: ErrorT AuthError Handler (Entity User)
  handleBasicAuth = do
    request <- lift waiRequest
    auth <- toErrorT $ maybeToEither AuthCredentialsNotSupplied $ lookup "Authorization" (requestHeaders request)
    (email, clearPassword) <- toErrorT $ mapLeft AuthMalformedCredentials $ parseAuthorizationHeader auth
    Entity _ email' <- ErrorT $ fmap (maybeToEither AuthCredentialsNotRecognised) $ runDB $ getBy $ UniqueEmail email
    userId <- toErrorT $ maybeToEither (AuthErrorOther "email has no associated user account!") $ emailUser email'
    user <- ErrorT $ fmap (maybeToEither $ AuthErrorOther "user account missing!") $ runDB $ get userId
    if maybe False (isValidPass clearPassword) (userPassword user)
      then return $ Entity userId user
      else throwError AuthCredentialsInvalid


unauthorized :: String -> GHandler sub master a
unauthorized err = sendResponseStatus HTTP.unauthorized401 $ RepPlain $ toContent $ T.pack err
