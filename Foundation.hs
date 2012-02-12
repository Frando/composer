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
import Control.Monad (liftM)
import Yesod
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Auth.Email
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
import Text.Hamlet (hamletFile, shamlet)
import Text.Blaze.Renderer.Text (renderHtml)
import Data.Text (Text)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
import Network.Mail.Mime (renderMail', simpleMail, Mail, Address(..))
#else
import Network.Mail.Mime (sendmail, renderMail', simpleMail, Mail, Address(..))
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
    mauth <- maybeAuth
    mUserIdentifier <- case mauth of
      Nothing -> return Nothing
      Just (Entity uid user) -> liftM Just $ getUserIdentifier uid user

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
        fmap Just $ insert $ mkUser (credsIdent creds)

  authPlugins _ = [authBrowserId, authGoogleEmail, authEmail]

  authHttpManager = httpManager

instance YesodAuthEmail Substantial where
  type AuthEmailId Substantial = EmailId

  addUnverified email verkey = do
    runDB $ insert $ Email email Nothing (Just verkey)
  
  sendVerifyEmail email _ verurl = do
    y <- getYesod
    mail <- liftIO $ simpleMail to emailFrom subject plain html []
    liftIO $ renderSendMail y mail
    where
      to = Address Nothing email
      subject = "Verify your email address"
      plain = "" -- TODO
      html = renderHtml [shamlet|
<p>Activate your Substantial account by clicking on the link below.
<p>
  <a href="#{verurl}">#{verurl}
|]

  getVerifyKey eid = runDB $ do
    liftM (maybe Nothing emailVerkey) $ get eid
  
  setVerifyKey eid verkey = runDB $ do
    update eid [EmailVerkey =. Just verkey]

  verifyAccount eid = runDB $ do
    me <- get eid
    case me of
      Nothing -> return Nothing
      Just e -> do
        uid <- insert $ mkUser (emailEmail e)
        update eid [EmailUser =. Just uid]
        return $ Just uid
  
  getPassword uid = runDB $ do
    liftM (maybe Nothing userPassword) $ get uid

  setPassword uid password = runDB $ do
    update uid [UserPassword =. Just password]

  getEmailCreds email = runDB $ do
     me <- getBy $ UniqueEmail email
     case me of
       Nothing -> return Nothing
       Just (Entity eid e) -> do
         case emailUser e of
           Nothing -> do
             return $ Just $ EmailCreds
               { emailCredsId = eid
               , emailCredsAuthId = Nothing
               , emailCredsStatus = False
               , emailCredsVerkey = emailVerkey e
               }
           Just uid -> do
             mu <- get uid
             return $ Just $ EmailCreds
               { emailCredsId = eid
               , emailCredsAuthId = Just uid
               , emailCredsStatus = case mu of
                   Just (User { userPassword = Just _ }) -> True
                   _ -> False
               , emailCredsVerkey = emailVerkey e
               }

  getEmail eid = runDB $ do
    liftM (fmap emailEmail) $ get eid

--mkUser :: Text -> User
mkUser ident = User
  { userIdent    = ident
  , userFullName = Nothing
  , userWebsite  = Nothing
  , userCompany  = Nothing
  , userLocation = Nothing
  , userPassword = Nothing
  }

getUserIdentifier :: UserId -> User -> GHandler sub Substantial Text
getUserIdentifier uid user = case userFullName user of
  Just fullName -> return fullName
  Nothing -> do
    musername <- runDB $ getBy $ UniqueUsernameUser uid
    case musername of
      Nothing -> return $ userIdent user
      Just username -> return $ usernameUsername (entityVal username)
    

emailFrom :: Address
emailFrom = Address Nothing "noreply@substantial.io"

renderSendMail :: Substantial -> Mail -> IO ()
renderSendMail s m = renderMail' m >>= deliver s

-- Sends off your mail. Requires sendmail in production!
deliver :: Substantial -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

instance RenderMessage Substantial FormMessage where
  renderMessage _ _ = defaultFormMessage
