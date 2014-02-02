module Widget.Expandy
  ( expandy
  , ExpandyState(..)
  ) where

import Prelude

import Data.Text (Text)
import Yesod
import Yesod.Default.Util (widgetFileNoReload)
import Yesod.Form.Jquery (YesodJquery (..))
import Data.Default (def)


data ExpandyState = Collapsed | Expanded


expandyIndicator :: ExpandyState -> Text
expandyIndicator Collapsed = "☞"
expandyIndicator Expanded = "☟"


{-expandy :: (YesodJquery master, MonadHandler m, MonadBaseControl IO m, master ~ HandlerSite m) => ExpandyState -> Text -> Text -> WidgetT master m ()-}
expandy initialState handleSelector' targetSelector' = do
  master <- handlerToWidget getYesod
  addScriptEither $ urlJqueryJs master

  widgetId <- handlerToWidget newIdent

  let handleSelector :: Text; handleSelector = handleSelector'
      targetSelector :: Text; targetSelector = targetSelector'
    in $(widgetFileNoReload def "expandy")
