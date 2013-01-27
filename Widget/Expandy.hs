module Widget.Expandy
  ( expandy
  , ExpandyState(..)
  ) where

import Prelude

import Data.Aeson (toJSON)
import Data.Default (def)
import Data.Text (Text)
import Yesod (GWidget, addScriptEither, getYesod, newIdent)
import Yesod.Default.Util (widgetFileNoReload)
import Yesod.Form.Jquery (YesodJquery (..))
import Yesod.Handler (lift)


data ExpandyState = Collapsed | Expanded


expandyIndicator :: ExpandyState -> Text
expandyIndicator Collapsed = "☞"
expandyIndicator Expanded = "☟"


expandy :: YesodJquery master => ExpandyState -> Text -> Text -> GWidget sub master ()
expandy initialState handleSelector targetSelector = do
  master <- lift getYesod
  addScriptEither $ urlJqueryJs master

  widgetId <- lift newIdent

  $(widgetFileNoReload def "expandy")
