module Handler.Templates where


import Import
import Util
import Util.Templates

import qualified Data.Char as Char
import Data.Monoid ((<>))
import Text.Blaze (Markup, toMarkup)


getTemplateOldTasksR, getTemplateTaskTrR, getTemplateNewSettingsR, getTemplateNewTasksTodayR, getTemplateNewTasksLaterR, getTemplateNewTasksDoneR, getTemplateNewTaskR :: Handler Html


getTemplateOldTasksR = $(wtemplateScheduleOptions "tasks/tasks-old")

getTemplateTaskTrR = $(templateFeatures "tasks/task-tr-ng")


getTemplateNewSettingsR = $(template "new-design/settings")
getTemplateNewTasksTodayR = $(templateScheduleOptions "new-design/tasks-today")

getTemplateNewTasksLaterR = $(template "new-design/tasks-later")
getTemplateNewTasksDoneR = $(template "new-design/tasks-done")
getTemplateNewTaskR = $(templateFeatures "new-design/task")


toScheduleId :: Schedule -> Markup
toScheduleId = toMarkup . ("schedule-" <>) . map Char.toLower . show
