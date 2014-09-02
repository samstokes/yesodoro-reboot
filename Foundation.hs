module Foundation
    ( App (..)
    , Client (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , putR, deleteR
    , newLayout
    , appTitle
    , module Settings
    , module Model
    , module Model.Note
    , module Model.Plan
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Jquery (YesodJquery(..))
import Yesod.Logger (Logger, logMsg, formatLogText, logLazyText)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.Development
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Model.Note
import Model.Plan
import Control.Applicative ((<$>))
import Control.Monad (join, when)
import Data.Maybe (isJust)
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Text.Shakespeare.Text (stext)
import Data.Text (Text)
import Data.Time (TimeZone(..))
import Util
import Util.Angular
import Util.SessionBackend

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , getClient :: Client
    }

-- Dummy subsite so our client-side routing can be type-safe too.
data Client = Client

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodSubData "Client" [] $(parseRoutesFile "config/routes-client")
mkYesodData "App" $(parseRoutesFile "config/routes")


putR :: Route App -> (Route App, [(Text, Text)])
putR = overrideMethodR "PUT"

deleteR :: Route App -> (Route App, [(Text, Text)])
deleteR = overrideMethodR "DELETE"


type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ secureClientSessionBackend key 120

    defaultLayout widget = do
        authed <- isJust <$> maybeAuthId
        when authed setXsrfCookie

        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addDefaultLayoutCss

            $(widgetFile "default-layout")

            addThirdPartyJs

            addAppJs

        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- For client-side routes, suppress the root completely: we just want the
    -- relative path (or ui-router can't join the dots).
    urlRenderOverride y (ClientR s) =
        Just $ uncurry (joinPath y "") $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifyInProd base64md5 Settings.staticDir (StaticR . flip StaticRoute [])
      where minifyInProd | production = minifym
                         | otherwise  = Right

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfHeadBlocking



newLayout :: Widget -> Handler RepHtml
newLayout widget = do
  authed <- isJust <$> maybeAuthId
  when authed setXsrfCookie

  mmsg <- getMessage
  title <- appTitle

  -- We break up the layout into two components:
  -- new-layout is the contents of the body tag, and
  -- new-layout-wrapper is the entire page. Since the final
  -- value passed to hamletToRepHtml cannot be a widget, this allows
  -- you to use normal widget features in new-layout.

  pc <- widgetToPageContent $ do
      $(widgetFile "new-layout")

      addThirdPartyJs

      addAppJs

  hamletToRepHtml $(hamletFile "templates/new-layout-wrapper.hamlet")


addDefaultLayoutCss :: GWidget sub App ()
addDefaultLayoutCss = do
  $(widgetFile "normalize")
  addStylesheet $ StaticR css_bootstrap_css


addThirdPartyJs :: GWidget sub App ()
addThirdPartyJs = do
  master <- lift getYesod

  addScriptEither $ urlJqueryJs master
  addScriptEither $ urlJqueryUiJs master
  addScriptEither $ eitherDev (StaticR js_vendor_angular_1_1_5_js) "//ajax.googleapis.com/ajax/libs/angularjs/1.1.5/angular.min.js"
  addScript $ StaticR js_vendor_showdown_js
  addScript $ StaticR js_vendor_angular_markdown_js
  addScript $ StaticR js_vendor_angular_ui_sortable_js
  addScript $ StaticR $ ifDev bower_components_angular_ui_router_release_angular_ui_router_js bower_components_angular_ui_router_release_angular_ui_router_min_js

  addScript $ StaticR bower_components_ng_group_src_ngGroup_js


addAppJs :: GWidget sub App ()
addAppJs = do
  addScript $ StaticR js_lib_functionBindPolyfill_js
  addScript $ StaticR js_lib_util_js

  addScript $ StaticR js_lib_expandy_js
  addStylesheet $ StaticR css_expandy_css

  addScript $ StaticR js_app_models_js
  addScript $ StaticR js_app_models_Task_js

  addScript $ StaticR js_app_services_js
  addScript $ StaticR js_app_services_billboard_js
  addScript $ StaticR js_app_services_exceptionHandler_js
  addScript $ StaticR js_app_services_httpErrorHandler_js
  addScript $ StaticR js_app_services_Tasks_js

  addScript $ StaticR js_app_controllers_js
  addScript $ StaticR js_app_controllers_PlansCtrl_js
  addScript $ StaticR js_app_controllers_TasksCtrl_js
  addScript $ StaticR js_app_controllers_NotesCtrl_js

  addScript $ StaticR js_app_directives_js
  addScript $ StaticR js_app_directives_billboard_js
  addScript $ StaticR js_app_directives_ssClickToEdit_js
  addScript $ StaticR js_app_directives_ssEditablePopup_js
  addScript $ StaticR js_app_directives_ssKeyboardDirectives_js
  addScript $ StaticR js_app_directives_ngBlur_js
  addScript $ StaticR js_app_directives_hibiIcon_js
  addScript $ StaticR js_app_directives_hibiTask_js

  addScript $ StaticR js_app_filters_js


appTitle :: Handler Text
appTitle = fmap (extraTitle . appExtra . settings) getYesod


instance YesodJquery App where
  urlJqueryJs _ = eitherDev (StaticR js_vendor_jquery_1_6_4_js) "//ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"
  urlJqueryUiJs _ = eitherDev (StaticR js_vendor_jquery_ui_1_8_17_custom_min_js) "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.17/jquery-ui.min.js"

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)


defaultTimeZone :: TimeZone
defaultTimeZone = read "PST"


instance YesodAuth App where
    type AuthId App = UserId

    redirectToReferer _ = True

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing ->
                  fmap Just $ insert $ User (credsIdent creds) Nothing defaultTimeZone noFlags

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = authGoogleEmail : ifDev [authEmail] []

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

instance YesodAuthEmail App where
    type AuthEmailId App = EmailId

    addUnverified email verkey =
        runDB $ insert $ Email email Nothing $ Just verkey

    sendVerifyEmail email _ verurl = do
        y <- getYesod
        -- Just log the verification URL for now, rather than emailing it...
        -- this is insecure and user-unfriendly, but only used in dev
        liftIO $ logLazyText (getLogger y) [stext|
Hello #{email}!
Please go to #{verurl}.
|]
    getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey eid key = runDB $ update eid [EmailVerkey =. Just key]
    verifyAccount eid = runDB $ do
        me <- get eid
        case me of
            Nothing -> return Nothing
            Just e -> do
                let email = emailEmail e
                case emailUser e of
                    Just uid -> return $ Just uid
                    Nothing -> do
                        uid <- insert $ User email Nothing defaultTimeZone noFlags
                        update eid [EmailUser =. Just uid, EmailVerkey =. Nothing]
                        return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        me <- getBy $ UniqueEmail email
        case me of
            Nothing -> return Nothing
            Just (Entity eid e) -> return $ Just EmailCreds
                { emailCredsId = eid
                , emailCredsAuthId = emailUser e
                , emailCredsStatus = isJust $ emailUser e
                , emailCredsVerkey = emailVerkey e
                }
    getEmail = runDB . fmap (fmap emailEmail) . get
