module Forms
  ( newTaskForm
  , editTaskForm
  , reorderTaskForm
  , newNoteForm
  ) where


import Import
import Util (fieldListOptions)

import Control.Monad (unless)


newTaskForm :: Bool -> Form NewTask
newTaskForm includeNonDaily = renderBootstrap $ NewTask
    <$> areq textField titleSettings Nothing
    <*> areq (radioFieldList scheduleListOptions) scheduleSettings (pure Once)
    <*> pure Nothing
  where
    titleSettings = fieldSettingsWithAttrs "Title" [("placeholder", "Add a task")]
    scheduleListOptions | includeNonDaily = fieldListOptions
                        | otherwise = filter (not . nonDaily . snd) fieldListOptions
    scheduleSettings = fieldSettingsWithAttrs "Schedule" [("class", "inline")]


editTaskForm :: Form TaskEdit
editTaskForm = renderDivs $ TaskTitleEdit <$> areq textField "Title" Nothing


reorderTaskForm :: Form TaskEdit
reorderTaskForm = renderDivs $ TaskOrderEdit <$> areq intField "Delta" Nothing


newNoteForm :: Form NewNote
newNoteForm = renderDivs $ NewNote <$> unTextarea <$> areq textareaField bodySettings Nothing
  where
    bodySettings = fieldSettingsWithAttrs "Body" [("placeholder", "Add a note")]


fieldSettingsWithAttrs :: SomeMessage master -> [(Text, Text)] -> FieldSettings master
fieldSettingsWithAttrs s = FieldSettings s Nothing Nothing Nothing
