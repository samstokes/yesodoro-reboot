module Forms
  ( newPlanForm
  , newTaskForm
  , editTaskForm
  , reorderTaskForm
  ) where


import Import
import Util (fieldListOptions)


newPlanForm :: Form NewPlan
newPlanForm = renderDivs $ NewPlan <$> unTextarea <$> areq textareaField "Body" Nothing


newTaskForm :: Form NewTask
newTaskForm = renderDivs $ NewTask
    <$> areq textField "Title" Nothing
    <*> areq (radioFieldList fieldListOptions) "Schedule" (pure Once)


editTaskForm :: Form TaskEdit
editTaskForm = renderDivs $ TaskTitleEdit <$> areq textField "Title" Nothing


reorderTaskForm :: Form TaskEdit
reorderTaskForm = renderDivs $ TaskOrderEdit <$> areq intField "Delta" Nothing
