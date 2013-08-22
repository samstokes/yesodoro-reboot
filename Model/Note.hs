{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Model.Note
    ( NewNote(..), createNote
    , taskNotes
    ) where

import Prelude
import Yesod

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON, (.:), parseJSON, toJSON)
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

instance ToJSON Note where
  toJSON note = object [
      "taskId" .= noteTask note
    , "body" .= noteBody note
    , "createdAt" .= noteCreatedAt note
    ]

instance FromJSON NewNote where
  parseJSON (Object o) = NewNote <$> (o .: "body")
  parseJSON v = fail $ "can't parse note: " ++ show v
