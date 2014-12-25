module Handler.Api where

import Import
import Util

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Network.Wai (requestHeaders)
import Yesod.Auth.Email (isValidPass)
import Yesod.Default.Config (AppConfig(..))

postApiTasksR :: Handler Value
postApiTasksR = do
  userId <- httpBasicAuth
  newTask <- requireJsonBody -- TODO error page is HTML, not friendly!

  existingTask <- runDB $ runMaybeT $ do
    extTask <- toMaybeT $ newTaskExt newTask
    updated <- lift $ syncExtTask userId extTask
    existingTask <- MaybeT $ findTaskByExtTask userId extTask
    return (updated, existingTask)

  case existingTask of
    Just (extUpdated, Entity taskId _) -> do
      (taskUpdated, _) <- runDB $ updateTask undefined (TaskSyncEdit newTask) taskId
      setLocation $ TaskR taskId
      returnJson $ object ["updated" .= (taskUpdated || extUpdated)]
    Nothing -> do
      Entity taskId _ <- runDB $ createTaskAtBottom userId newTask
      sendResponseCreated $ TaskR taskId


getApiExtTasksForSourceR :: ExternalSourceName -> Handler Value
getApiExtTasksForSourceR source = do
  userId <- httpBasicAuth
  extTasks <- runDB $ allExtTasksForSource userId source
  returnJson $ map entityVal extTasks


setLocation :: Route App -> Handler ()
setLocation url = do
  r <- getUrlRender
  addHeader "Location" $ r url


data AuthError = AuthCredentialsNotSupplied
               | AuthMalformedCredentials String
               | AuthCredentialsNotRecognised
               | AuthCredentialsInvalid
               | AuthErrorOther String
  deriving (Show)


httpBasicAuth :: Handler UserId
httpBasicAuth = do
    root <- (appRoot . settings) <$> getYesod
    auth <- runExceptT handleBasicAuth
    $logDebug $ T.pack $ "HTTP Basic auth: " ++ show auth
    case auth of
      Right userId -> return userId

      Left AuthCredentialsNotSupplied -> do
        addHeader "WWW-Authenticate" $ T.concat ["Basic Realm=\"", root, "\""]
        permissionDenied "Authentication required"
      Left (AuthMalformedCredentials err) ->
        sendResponseStatus HTTP.badRequest400 $ RepPlain $ toContent $ T.pack err
      Left AuthCredentialsNotRecognised -> unauthorized "credentials not recognised"
      Left AuthCredentialsInvalid -> unauthorized "credentials not recognised"
      Left (AuthErrorOther err) -> do
        $logWarn $ T.pack $ "unexpected auth error: " ++ err
        unauthorized "credentials not recognised"
  where
  handleBasicAuth :: ExceptT AuthError Handler UserId
  handleBasicAuth = do
    request <- lift waiRequest
    auth <- toExceptT $ maybeToEither AuthCredentialsNotSupplied $ lookup "Authorization" (requestHeaders request)
    (email, clearPassword) <- toExceptT $ mapLeft AuthMalformedCredentials $ parseAuthorizationHeader auth
    Entity _ email' <- ExceptT $ fmap (maybeToEither AuthCredentialsNotRecognised) $ runDB $ getBy $ UniqueEmail email
    userId <- toExceptT $ maybeToEither (AuthErrorOther "email has no associated user account!") $ emailUser email'
    user <- ExceptT $ fmap (maybeToEither $ AuthErrorOther "user account missing!") $ runDB $ get userId
    if maybe False (isValidPass clearPassword) (userPassword user)
      then return userId
      else throwE AuthCredentialsInvalid


unauthorized :: String -> Handler a
unauthorized err = sendResponseStatus HTTP.unauthorized401 $ RepPlain $ toContent $ T.pack err
