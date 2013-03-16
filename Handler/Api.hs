module Handler.Api where

import Import
import Util

import Control.Monad.Trans.Error
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Network.Wai (requestHeaders)
import Yesod.Auth.Email (isValidPass)
import Yesod.Default.Config (AppConfig(..))

postApiTasksR :: Handler RepJson
postApiTasksR = do
  userId <- httpBasicAuth
  newTask <- parseJsonBody_ -- TODO error page is HTML, not friendly!

  taskId <- runDB $ createTaskAtBottom userId newTask
  sendResponseCreated $ TaskR taskId


data AuthError = AuthCredentialsNotSupplied
               | AuthMalformedCredentials String
               | AuthCredentialsNotRecognised
               | AuthCredentialsInvalid
               | AuthErrorOther String
  deriving (Show)

instance Error AuthError where
  strMsg = AuthErrorOther


httpBasicAuth :: Handler UserId
httpBasicAuth = do
    root <- (appRoot . settings) <$> getYesod
    auth <- runErrorT handleBasicAuth
    $logDebug $ T.pack $ "HTTP Basic auth: " ++ show auth
    case auth of
      Right userId -> return userId

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
  handleBasicAuth :: ErrorT AuthError Handler UserId
  handleBasicAuth = do
    request <- lift waiRequest
    auth <- toErrorT $ maybeToEither AuthCredentialsNotSupplied $ lookup "Authorization" (requestHeaders request)
    (email, clearPassword) <- toErrorT $ mapLeft AuthMalformedCredentials $ parseAuthorizationHeader auth
    Entity _ email' <- ErrorT $ fmap (maybeToEither AuthCredentialsNotRecognised) $ runDB $ getBy $ UniqueEmail email
    userId <- toErrorT $ maybeToEither (AuthErrorOther "email has no associated user account!") $ emailUser email'
    user <- ErrorT $ fmap (maybeToEither $ AuthErrorOther "user account missing!") $ runDB $ get userId
    if maybe False (isValidPass clearPassword) (userPassword user)
      then return userId
      else throwError AuthCredentialsInvalid


unauthorized :: String -> GHandler sub master a
unauthorized err = sendResponseStatus HTTP.unauthorized401 $ RepPlain $ toContent $ T.pack err
