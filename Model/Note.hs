{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Note
    ( NewNote(..), createNote
    , taskNotes
    ) where

import Prelude
import Yesod

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types (ToJSON, toJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.GenericSql (SqlPersist)
import Model
import Util


taskNotes :: PersistQuery SqlPersist m => [SelectOpt Note] -> TaskId -> SqlPersist m [Entity Note]
taskNotes opts taskId = selectList [NoteTask ==. taskId] opts


data NewNote = NewNote { newNoteBody :: Text } deriving (Show)

newNote :: TaskId -> UTCTime -> NewNote -> Note
newNote taskId createdAt (NewNote body) = Note {
    noteTask = taskId
  , noteBody = body
  , noteCreatedAt = createdAt
  }

createNote :: (MonadIO m, PersistQuery SqlPersist m) => TaskId -> NewNote -> SqlPersist m (Entity Note)
createNote taskId note = do
  time <- now
  let note' = newNote taskId time note
  noteId <- insert note'
  return $ Entity noteId note'

instance ToJSON (NoteGeneric b) where
  toJSON note = object [
      ("taskId", toJSON $ noteTask note)
    , ("body", toJSON $ noteBody note)
    , ("createdAt", toJSON $ noteCreatedAt note)
    ]

instance ToJSON (Entity Note) where
  toJSON (Entity k n) = object ["id" .= k, "note" .= n]
