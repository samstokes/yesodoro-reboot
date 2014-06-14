module Handler.Client (
  State(..)
, states
, stateDeclJavascript
, statesDeclJavascript
) where

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
      templateUrl: '@{stateTemplateR state}'
    })
|]

statesDeclJavascript :: (Route App -> [param] -> Text) -> Javascript
statesDeclJavascript render = foldMap decl states
  where decl = flip stateDeclJavascript render
