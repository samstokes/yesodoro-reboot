module Handler.Templates where


import Import
import Util
import Util.Templates
import Widget.TaskList

import qualified Data.Char as Char
import Data.Monoid ((<>))
import Text.Blaze (Markup, toMarkup)


getTemplateOldTasksR, getTemplateOldTaskTrR, getTemplateHomeR, getTemplateNewSettingsR, getTemplateNewTasksTodayR, getTemplateNewTasksLaterR, getTemplateNewTasksDoneR, getTemplateNewTaskR :: HandlerT Templates Handler Html


getTemplateOldTasksR = $(wtemplateScheduleOptions "tasks/tasks-old")

getTemplateOldTaskTrR = $(templateFeatures "tasks/task-tr-ng")


getTemplateHomeR = $(wtemplate "homepage/home")


getTemplateNewSettingsR = $(template "new-design/settings")
getTemplateNewTasksTodayR = $(templateScheduleOptions "new-design/tasks-today")

getTemplateNewTasksLaterR = $(template "new-design/tasks-later")
getTemplateNewTasksDoneR = $(template "new-design/tasks-done")
getTemplateNewTaskR = $(templateFeatures "new-design/task")


toScheduleId :: Schedule -> Markup
toScheduleId = toMarkup . ("schedule-" <>) . map Char.toLower . show
