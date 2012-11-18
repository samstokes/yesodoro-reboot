module Forms
  ( newPlanForm
  , newTaskForm
  , editTaskForm
  , reorderTaskForm
  , newNoteForm
  ) where


import Import
import Util (fieldListOptions)


newPlanForm :: Form NewPlan
newPlanForm = renderDivs $ NewPlan <$> areq textField bodySettings Nothing
  where
    bodySettings = fieldSettingsWithAttrs "Body" [("placeholder", "Add a plan")]


newTaskForm :: Form NewTask
newTaskForm = renderDivs $ NewTask
    <$> areq textField titleSettings Nothing
    <*> areq (radioFieldList fieldListOptions) "Schedule" (pure Once)
  where
    titleSettings = fieldSettingsWithAttrs "Title" [("placeholder", "Add a task")]


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
