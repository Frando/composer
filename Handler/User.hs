module Handler.User
  ( getUserR
  ) where

import Import
import Data.Text.Read (decimal)

getUserByIdent :: Text -> Handler (Either Text (Maybe Text, UserId, User))
getUserByIdent ident = case decimal ident :: Either String (Int, Text) of
  Right (_, "") -> runDB $ do
    case fromPathPiece ident of
      Nothing -> lift notFound
      Just uid -> do
        u <- get404 uid
        -- maybe user name entity
        mune <- getBy $ UniqueUsernameUser uid
        return $ case mune of
          Just une -> Left $ usernameUsername (entityVal une)
          Nothing -> Right (Nothing, uid, u)
  _ -> runDB $ do
    une <- getBy404 $ UniqueUsername ident
    let uid = usernameUser $ entityVal une
    u <- get404 uid
    return $ Right (Just ident, uid, u)

getUserR :: Text -> Handler RepHtml
getUserR ident = do
  euser <- getUserByIdent ident
  case euser of
    Left username -> redirect $ UserR username
    Right (musername, uid, user) -> do
      memail <- fmap (fmap $ emailEmail . entityVal) $ runDB $ selectFirst [EmailUser ==. Just uid] []
      let identifier = maybe (toHtml $ show uid) toHtml $ musername `mplus` memail
      defaultLayout $ do
        setTitle "User page"
        $(widgetFile "user")