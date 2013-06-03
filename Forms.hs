module Forms
  ( newTaskForm
  , editTaskForm
  , reorderTaskForm
  , newNoteForm
  ) where


import Import
import Util (fieldListOptions)

import Control.Monad (unless)


newTaskForm :: Form NewTask
newTaskForm = renderBootstrap $ NewTask
    <$> areq textField titleSettings Nothing
    <*> areq (radioFieldList' fieldListOptions) scheduleSettings (pure Once)
    <*> pure Nothing
  where
    titleSettings = fieldSettingsWithAttrs "Title" [("placeholder", "Add a task")]
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


-- backported from yesod 28e0bf8 (yesod-form 1.1.0.1)
radioFieldList' :: (Eq a, RenderMessage master FormMessage, RenderMessage master msg) => [(msg, a)] -> Field sub master a
radioFieldList' = radioField' . optionsPairs

radioField' :: (Eq a, RenderMessage master FormMessage) => GHandler sub master (OptionList a) -> Field sub master a
radioField' = selectFieldHelper
    (\theId _name inside -> [whamlet|
<div ##{theId}>^{inside}
|])
    (\theId name isSel -> [whamlet|
<label .radio for=#{theId}-none>
    <div>
        <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
        _{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
<label .radio for=#{theId}-#{value}>
    <div>
        <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
        \#{text}
|])


selectFieldHelper
        :: (Eq a, RenderMessage master FormMessage)
        => (Text -> Text -> GWidget sub master () -> GWidget sub master ())
        -> (Text -> Text -> Bool -> GWidget sub master ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> GWidget sub master ())
        -> GHandler sub master (OptionList a) -> Field sub master a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ lift opts'
        outside theId name $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                attrs
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
                (optionDisplay opt)
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y
