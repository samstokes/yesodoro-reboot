module Handler.Api where

import Import
import Util

import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Network.Wai (requestHeaders)
import Yesod.Auth.Email (isValidPass)

postApiTasksR :: Handler RepJson
postApiTasksR = do
  userId <- httpBasicAuth
  newTask <- parseJsonBody_ -- TODO error page is HTML, not friendly!

  taskId <- runDB $ createTaskAtBottom userId newTask
  sendResponseCreated $ TaskR taskId


httpBasicAuth :: Handler UserId
httpBasicAuth = do
  request <- waiRequest
  -- TODO uck refactor this
  case lookup "Authorization" (requestHeaders request) of
    Just auth -> case parseAuthorizationHeader auth of
      Right (email, clearPassword) -> do
        memail <- runDB $ getBy $ UniqueEmail email
        case memail of
          Just (Entity _ email') -> do
            let mUserId = emailUser email'
            muser <- runDB $ fromMaybe (return Nothing) $ get <$> mUserId
            case muser of
              Just user ->
                if maybe False (isValidPass clearPassword) (userPassword user)
                  then return $ fromJust mUserId
                  else unauthorized
              Nothing -> unauthorized
          Nothing -> unauthorized
      Left err -> sendResponseStatus HTTP.badRequest400 $ RepPlain $ toContent $ T.pack err
    _ -> do
      setHeader "WWW-Authenticate" "Basic Realm=\"My Realm\"" -- TODO
      permissionDenied "Authentication required"


unauthorized :: GHandler sub master a
unauthorized = sendResponseStatus HTTP.unauthorized401 $ RepPlain $ toContent $ T.pack "sorry"
