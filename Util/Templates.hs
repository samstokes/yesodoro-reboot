module Util.Templates (
  template
, wtemplate
, templateFeatures
, wtemplateFeatures
, templateScheduleOptions
, wtemplateScheduleOptions
) where

import Import
import Util.Angular
import Util.Http

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default
import Language.Haskell.TH
import Text.Hamlet (hamletFile)


data TemplateComplexity = Mild | HasFeatures | HasScheduleOptions

data TemplateSettings = TemplateSettings {
    tsHasWidgets :: Bool
  , tsComplexity :: TemplateComplexity
  }

instance Default TemplateSettings where def = TemplateSettings {
    tsHasWidgets = False
  , tsComplexity = Mild
  }

template, wtemplate, templateFeatures, wtemplateFeatures, templateScheduleOptions, wtemplateScheduleOptions :: String -> Q Exp
template = templateWithSettings def
wtemplate = templateWithSettings def { tsHasWidgets = True }
templateFeatures = templateWithSettings def { tsComplexity = HasFeatures }
wtemplateFeatures = templateWithSettings def { tsHasWidgets = True, tsComplexity = HasFeatures }
templateScheduleOptions = templateWithSettings def { tsComplexity = HasScheduleOptions }
wtemplateScheduleOptions = templateWithSettings def { tsHasWidgets = True, tsComplexity = HasScheduleOptions }

templateWithSettings :: TemplateSettings -> String -> Q Exp
templateWithSettings ts name = tws $ tsComplexity ts
  where
    -- TODO serve some caching header here too
    tws Mild = templateToHtml
    tws HasFeatures = [|do
      features <- featureSettings . fmap entityVal <$> maybeNgAuth
      let has feature = hasFlag feature features
      withDigestEtag (featuresEtag features) $templateToHtml
      |]
    tws HasScheduleOptions = [|do
      features <- featureSettings . fmap entityVal <$> maybeNgAuth
      let
        has feature = hasFlag feature features
        scheduleOptions = if has FeatureNonDailySchedules
          then schedules
          else filter (not . nonDaily) schedules
      withDigestEtag (featuresEtag features) $templateToHtml
      |]
    filePath = "templates/" ++ name ++ ".hamlet"
    templateToHtml | tsHasWidgets ts = [|do
        pc <- widgetToPageContent $(whamletFile filePath)
        giveUrlRenderer [hamlet|^{pageBody pc}|]
      |]
                   | otherwise       = [|giveUrlRenderer $(hamletFile filePath)|]

featuresEtag :: Flags Feature -> Handler B.ByteString
featuresEtag features = return . B.pack . concat $ [
    show estimateOptions
  , show features
  ]
