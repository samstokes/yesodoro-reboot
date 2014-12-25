{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Note
    ( NewNote(..), createNote
    , taskNotes
    ) where

import Prelude
import Yesod

import Control.Applicative
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql (SqlPersistT)
import Model
import Util


taskNotes :: PersistQuery (SqlPersistT m) => [SelectOpt Note] -> TaskId -> SqlPersistT m [Entity Note]
taskNotes opts taskId = selectList [NoteTask ==. taskId] opts


data NewNote = NewNote { newNoteBody :: Text } deriving (Show)

newNote :: TaskId -> UTCTime -> NewNote -> Note
newNote taskId createdAt (NewNote body) = Note {
    noteTask = taskId
  , noteBody = body
  , noteCreatedAt = createdAt
  }

createNote :: (MonadIO m, PersistQuery (SqlPersistT m)) => TaskId -> NewNote -> SqlPersistT m (Entity Note)
createNote taskId note = do
  time <- now
  let note' = newNote taskId time note
  noteId <- insert note'
  return $ Entity noteId note'

instance ToJSON Note where
  toJSON note = object [
      ("taskId", toJSON $ noteTask note)
    , ("body", toJSON $ noteBody note)
    , ("createdAt", toJSON $ noteCreatedAt note)
    ]

instance FromJSON NewNote where
  parseJSON (Object o) = NewNote <$> (o .: "body")
  parseJSON v = fail $ "can't parse note: " ++ show v

instance ToJSON (Entity Note) where
  toJSON (Entity k n) = object ["id" .= k, "note" .= n]
