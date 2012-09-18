module Forms
  ( newTaskForm
  , editTaskForm
  , reorderTaskForm
  ) where


import Import


newTaskForm :: Form NewTask
newTaskForm = renderDivs $ NewTask <$> areq textField "Title" Nothing


editTaskForm :: Form TaskEdit
editTaskForm = renderDivs $ TaskTitleEdit <$> areq textField "Title" Nothing


reorderTaskForm :: Form TaskEdit
reorderTaskForm = renderDivs $ TaskOrderEdit <$> areq intField "Delta" Nothing
