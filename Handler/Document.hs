module Handler.Document
  ( getNewDocumentR
  , postNewDocumentR
  , getDocumentR
  ) where

import Import
import Data.Text (pack)
import Control.Arrow ((&&&))

newDocumentForm :: Document -> Html -> MForm Substantial Substantial (FormResult Document, Widget)
newDocumentForm doc = renderTable $ Document
  <$> areq textField "Title" (Just $ documentTitle doc)
  <*> areq (radioFieldList publishSettings) "Publish" (Just $ documentPublishSettings doc)
  where
    publishSettings = map (pack . show &&& id) $ [minBound..maxBound]

defaultDocument :: Document
defaultDocument = Document "" PublishedVersionsOnly 

getNewDocumentR :: Handler RepHtml
getNewDocumentR = do
  uid <- requireAuthId
  ((res, newDocumentFormWidget), encoding) <- runFormPost $ newDocumentForm defaultDocument
  case res of
    FormSuccess doc -> do
      docid <- runDB $ do
        docid <- insert doc
        _ <- insert $ Permission uid docid Author
        return docid
      redirect $ DocumentR docid
    _ -> return ()
  defaultLayout $ do
    setTitle "New Document"
    $(widgetFile "new-document")

postNewDocumentR :: Handler RepHtml
postNewDocumentR = getNewDocumentR

getDocumentR :: DocumentId -> Handler RepHtml
getDocumentR docid = do
  doc <- runDB $ get404 docid
  defaultLayout $ do
    addStylesheetRemote "../../modules/ve/es/styles/ve.es.Document.css"
    addStylesheetRemote "../../modules/ve/es/styles/ve.es.Content.css"
    addStylesheetRemote "../../modules/ve/es/styles/ve.es.Surface.css"
    addStylesheetRemote "../../modules/ve/ui/styles/ve.ui.Context.css"
    addStylesheetRemote "../../modules/ve/ui/styles/ve.ui.Inspector.css"
    addStylesheetRemote "../../modules/ve/ui/styles/ve.ui.Menu.css"
    addStylesheetRemote "../../modules/ve/ui/styles/ve.ui.Toolbar.css"
    addStylesheetRemote "../../modules/sandbox/sandbox.css"
    setTitle "Document"
    $(widgetFile "document")