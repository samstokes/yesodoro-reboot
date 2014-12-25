module Handler.Notes where

import Import
import Util.Angular

postTaskNotesR :: TaskId -> Handler Value
postTaskNotesR taskId = do
  _ <- authedTask taskId
  newNote <- requireJsonBody -- TODO error page is HTML, not friendly!
  noteEntity <- runDB $ createNote taskId newNote
  returnJson noteEntity


authedTask :: TaskId -> Handler (Entity Task)
authedTask taskId = do
  userId <- requireNgAuthId
  maybeAuthedTask <- runDB $ selectFirst [TaskId ==. taskId, TaskUser ==. userId] []
  case maybeAuthedTask of
    Just plan -> return plan
    Nothing -> notFound
