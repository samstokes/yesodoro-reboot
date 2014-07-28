module Handler.Client (
  State(..)
, states
, stateDeclJavascript
, statesDeclJavascript
) where

import Data.Monoid ((<>))
import Data.Aeson (ToJSON(..))
import Data.Foldable (foldMap)
import Data.String (IsString)
import Text.Blaze (ToMarkup(..))
import Text.Julius (Javascript)

import Import
import Prelude

data State =
    StateSettings
  | StateTasksToday
  | StateTasksLater
  | StateTasksDone
  deriving (Show, Enum, Bounded)

states :: [State]
states = [minBound .. maxBound]

stateName :: IsString s => State -> s
stateName StateSettings = "settings"
stateName StateTasksToday = "tasks-today"
stateName StateTasksLater = "tasks-later"
stateName StateTasksDone = "tasks-done"

stateClientR :: State -> Route Client
stateClientR StateSettings = NewSettingsR
stateClientR StateTasksToday = NewTasksTodayR
stateClientR StateTasksLater = NewTasksLaterR
stateClientR StateTasksDone = NewTasksDoneR

stateR :: State -> Route App
stateR = ClientR . stateClientR

stateController :: State -> Maybe Text
stateController StateTasksToday = Just "TasksCtrl"
stateController StateTasksDone = Just "TasksCtrl"
stateController _ = Nothing

stateResolves :: State -> [(Text, t -> Javascript)]
stateResolves StateTasksToday = [
    ("tasks", [julius|function (Tasks) { return Tasks.all(); }|])
  ]
stateResolves StateTasksDone = [
    ("tasks", [julius|function (Tasks) { return Tasks.all(); }|])
  ]
stateResolves _ = []

stateTemplateR :: State -> Route App
stateTemplateR StateSettings = TemplateNewSettingsR
stateTemplateR StateTasksToday = TemplateNewTasksTodayR
stateTemplateR StateTasksLater = TemplateNewTasksLaterR
stateTemplateR StateTasksDone = TemplateNewTasksDoneR

instance ToMarkup State where
  toMarkup = stateName

instance ToJSON State where
  toJSON = stateName


stateDeclJavascript :: State -> (Route App -> [param] -> Text) -> Javascript
stateDeclJavascript state = [julius|
    .state(#{toJSON state}, {
      url: '@{stateR state}',
      ^{controllerDecl (stateController state)}
      ^{resolvesDecl (stateResolves state)}
      templateUrl: '@{stateTemplateR state}'
    })
|] where
  controllerDecl (Just ctrl) = [julius|controller: #{toJSON ctrl},|]
  controllerDecl Nothing = mempty
  resolvesDecl [] = mempty
  resolvesDecl resolves = [julius|resolve: {
        ^{foldMap resolveDecl resolves}
      },|]
  resolveDecl (key, resolve) = [julius|#{key}: ^{resolve}|]

statesDeclJavascript :: (Route App -> [param] -> Text) -> Text -> Javascript
statesDeclJavascript render stateProvider = header <> foldMap decl states
  where
    header = [julius|#{stateProvider}|] render
    decl = flip stateDeclJavascript render
