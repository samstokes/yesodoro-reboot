module Handler.Notes where

import Import
import Util.Angular

postTaskNotesR :: TaskId -> Handler RepJson
postTaskNotesR taskId = do
  _ <- authedTask taskId
  newNote <- parseJsonBody_ -- TODO error page is HTML, not friendly!
  noteEntity <- runDB $ createNote taskId newNote
  jsonToRepJson noteEntity


authedTask :: TaskId -> Handler (Entity Task)
authedTask taskId = do
  userId <- requireAuthIdPreventingXsrf
  maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
  case maybeAuthedTask of
    Just plan -> return plan
    Nothing -> notFound
