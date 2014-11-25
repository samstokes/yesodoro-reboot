module Widget.TaskList
  ( widgetTasksToday
  ) where


import qualified Data.Char as Char
import Data.Monoid ((<>))
import Text.Blaze (Markup, toMarkup)

import Import


widgetTasksToday :: Widget
widgetTasksToday = do
  features <- featureSettings . fmap entityVal <$> handlerToWidget maybeAuth
  let
    has feature = hasFlag feature features
    scheduleOptions = if has FeatureNonDailySchedules
      then schedules
      else filter (not . nonDaily) schedules
  $(widgetFile "new-design/tasks-today")


toScheduleId :: Schedule -> Markup
toScheduleId = toMarkup . ("schedule-" <>) . map Char.toLower . show
