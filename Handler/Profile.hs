module Handler.Profile
  ( getProfileR
  , postProfileR
  ) where

import Import

userForm :: User -> Html -> MForm Substantial Substantial (FormResult User, Widget)
userForm user = renderTable $ User
  <$> pure (userIdent user)
  <*> aopt textField "Full name" (Just $ userFullName user)
  <*> aopt urlField "Website" (Just $ userWebsite user)
  <*> aopt textField "Company" (Just $ userCompany user)
  <*> aopt textField "Location" (Just $ userLocation user)
  <*> pure (userPassword user)

getProfileR :: Handler RepHtml
getProfileR = do
  Entity uid user <- requireAuth
  ((res, formWidget), enctype) <- runFormPost $ userForm user
  case res of
    FormSuccess user' -> do
      runDB $ replace uid user'
      setMessage "Updated your profile"
      redirect ProfileR
    _ -> return ()
  defaultLayout $ do
    setTitle "User Profile"
    $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR