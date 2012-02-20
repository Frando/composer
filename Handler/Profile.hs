module Handler.Profile
  ( getProfileR
  , postProfileR
  , postSetUsernameR
  ) where

import Import
import Yesod.Auth.Email (setpassR)
import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum)
import Helper.Gravatar (maybeGravatar)
import Data.Maybe (fromMaybe)

userForm :: User -> Html -> MForm Substantial Substantial (FormResult User, Widget)
userForm user = renderTable $ User
  <$> pure (userIdent user)
  <*> aopt textField "Full name" (Just $ userFullName user)
  <*> aopt urlField "Website" (Just $ userWebsite user)
  <*> aopt textField "Company" (Just $ userCompany user)
  <*> aopt textField "Location" (Just $ userLocation user)
  <*> pure (userPassword user)

usernameForm :: Maybe Text -> Html -> MForm Substantial Substantial (FormResult Text, Widget)
usernameForm musername = renderTable $
  areq (checkBool isValidUsername  ("Not a valid username" :: Text) textField) "Username" musername

getProfileR :: Handler RepHtml
getProfileR = do
  Entity uid user <- requireAuth
  ((res, formWidget), userEnctype) <- runFormPost $ userForm user
  case res of
    FormSuccess user' -> do
      runDB $ replace uid user'
      setMessage "Updated your profile"
      redirect ProfileR
    _ -> return ()
  musername <- runDB $ do
    liftM (fmap $ usernameUsername . entityVal) $ getBy $ UniqueUsernameUser uid
  ((_, usernameFormWidget), usernameEnctype) <- generateFormPost $ usernameForm musername
  memail <- runDB $ do
    liftM (fmap $ emailEmail . entityVal) $ selectFirst [EmailUser ==. Just uid] []
  let userIdentifier = fromMaybe (toPathPiece uid) musername
  defaultLayout $ do
    setTitle "User Profile"
    $(widgetFile "profile")

isValidUsername :: Text -> Bool
isValidUsername un =  T.length un >= 1
                   && validFirstChar (T.head un)
                   && T.all validRestChar (T.tail un)
  where
    validFirstChar c = isAlpha c    || c `elem` specialChars
    validRestChar  c = isAlphaNum c || c `elem` specialChars
    specialChars = "_"

postProfileR :: Handler RepHtml
postProfileR = getProfileR

postSetUsernameR :: Handler ()
postSetUsernameR = do
  uid <- requireAuthId
  ((res, _), _) <- runFormPost $ usernameForm Nothing
  case res of 
    FormMissing -> redirect ProfileR
    FormFailure _ -> do
      setMessage "Invalid username"
      redirect ProfileR
    FormSuccess username -> do
      _ <- runDB $ do
        deleteBy $ UniqueUsernameUser uid
        insert $ Username uid username
      redirect ProfileR