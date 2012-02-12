module Foundation
  ( Substantial (..)
  , Route (..)
  , SubstantialMessage (..)
  , resourcesSubstantial
  , Handler
  , Widget
  , Form
  , maybeAuth
  , requireAuth
  , module Settings
  , module Model
  ) where

import Prelude
import Yesod
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

data Substantial = Substantial
  { settings :: AppConfig DefaultEnv Extra
  , getLogger :: Logger
  , getStatic :: Static -- ^ Settings for static file serving.
  , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
  , httpManager :: Manager
  , persistConfig :: Settings.PersistConfig
  }

mkMessage "Substantial" "messages" "en"

mkYesodData "Substantial" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Substantial Substantial (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Substantial where
  approot = ApprootMaster $ appRoot . settings

  encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR bootstrap_css_bootstrap_css
      $(widgetFile "default-layout")
    hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- This is done to provide an optimization for serving static files from
  -- a separate domain. Please see the staticRoot setting in Settings.hs
  urlRenderOverride y (StaticR s) =
    Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
  urlRenderOverride _ _ = Nothing

  authRoute _ = Just $ AuthR LoginR

  messageLogger y loc level msg =
    formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

  yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

instance YesodPersist Substantial where
  type YesodPersistBackend Substantial = SqlPersist
  runDB f = do
    master <- getYesod
    Database.Persist.Store.runPool
      (persistConfig master)
      f
      (connPool master)

instance YesodAuth Substantial where
  type AuthId Substantial = UserId

  loginDest _ = RootR
  logoutDest _ = RootR

  getAuthId creds = runDB $ do
    x <- getBy $ UniqueUser $ credsIdent creds
    case x of
      Just (Entity uid _) -> return $ Just uid
      Nothing -> do
        fmap Just $ insert $ User (credsIdent creds) Nothing

  -- You can add other plugins like BrowserID, email or OAuth here
  authPlugins _ = [authBrowserId, authGoogleEmail]

  authHttpManager = httpManager

-- Sends off your mail. Requires sendmail in production!
deliver :: Substantial -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

instance RenderMessage Substantial FormMessage where
  renderMessage _ _ = defaultFormMessage
