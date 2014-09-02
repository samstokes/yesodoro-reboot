module Handler.Client (
  State(..)
, states
, stateDeclJavascript
, statesDeclJavascript
) where

import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Data.String (IsString)
import Text.Blaze (ToMarkup(..))
import Text.Julius (Javascript, rawJS)

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
stateController StateSettings = Just "SettingsCtrl"
stateController StateTasksToday = Just "TasksCtrl"
stateController StateTasksLater = Just "TasksCtrl"
stateController StateTasksDone = Just "TasksCtrl"

stateResolves :: State -> [(Text, (url -> [(Text, Text)] -> Text) -> Javascript)]
stateResolves StateSettings = [
    ("loggedIn", [julius|function (Settings) { return Settings.isAuthed(); }|])
  ]
stateResolves StateTasksToday = [
    ("tasks", [julius|function (Tasks) { return Tasks.today(); }|])
  ]
stateResolves StateTasksLater = [
    ("tasks", [julius|function (Tasks) { return Tasks.later(); }|])
  ]
stateResolves StateTasksDone = [
    ("tasks", [julius|function ($stateParams, Tasks) {
      var days = $stateParams.days;
      if (days !== null) {
        if (/^[0-9]+$/.test(days)) {
          return Tasks.done(Number(days));
        } else {
          throw "days must be numeric!";
        }
      }
      return Tasks.done();
    }|])
  ]

stateTemplateR :: State -> Route App
stateTemplateR StateSettings = TemplateNewSettingsR
stateTemplateR StateTasksToday = TemplateNewTasksTodayR
stateTemplateR StateTasksLater = TemplateNewTasksLaterR
stateTemplateR StateTasksDone = TemplateNewTasksDoneR

instance ToMarkup State where
  toMarkup = stateName

instance ToJSON State where
  toJSON = stateName


stateDeclJavascript :: State -> (Route App -> [(Text, Text)] -> Text) -> Javascript
stateDeclJavascript state = [julius|
    .state(#{toJSON state}, {
      url: '@{stateR state}?days',
      ^{controllerDecl (stateController state)}
      ^{resolvesDecl (stateResolves state)}
      templateUrl: '@{stateTemplateR state}'
    })
|] where
  controllerDecl (Just ctrl) = [julius|controller: #{toJSON ctrl},|]
  controllerDecl Nothing = mempty
  resolvesDecl resolves = [julius|resolve: {
        ^{foldMap resolveDecl resolves}
        hibiUi: function () { return 'new'; }
      },|]
  resolveDecl (key, resolve) = [julius|#{toJSON key}: ^{resolve},|]

statesDeclJavascript :: (Route App -> [(Text, Text)] -> Text) -> Text -> Javascript
statesDeclJavascript render stateProvider = header <> foldMap decl states
  where
    header = [julius|#{rawJS stateProvider}|] render
    decl = flip stateDeclJavascript render
