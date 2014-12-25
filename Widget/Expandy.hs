module Widget.Expandy
  ( expandy
  , ExpandyState(..)
  ) where

import Prelude

import Data.Default (def)
import Data.Text (Text)
import Yesod (WidgetT, addScriptEither, getYesod, newIdent)
import Yesod.Default.Util (widgetFileNoReload)
import Yesod.Form.Jquery (YesodJquery (..))


data ExpandyState = Collapsed | Expanded


expandyIndicator :: ExpandyState -> Text
expandyIndicator Collapsed = "☞"
expandyIndicator Expanded = "☟"


expandy :: YesodJquery site => ExpandyState -> Text -> Text -> WidgetT site IO ()
expandy initialState handleSelector targetSelector = do
  master <- getYesod
  addScriptEither $ urlJqueryJs master

  widgetId <- newIdent

  $(widgetFileNoReload def "expandy")
