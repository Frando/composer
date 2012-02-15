module Handler.Document
  ( getNewDocumentR
  , postNewDocumentR
  , getDocumentR
  , getDocumentTransactionsR
  , postDocumentTransactionsR
  ) where

import Import
import Data.Text (pack)
import Control.Arrow ((&&&))
import Control.Concurrent.MVar (modifyMVar)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import qualified Data.Map as M
import Network.Wai (requestBody)
import Data.Conduit (runResourceT, ($$))
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Blaze.ByteString.Builder.ByteString (fromByteString)

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

  let modeWikitext = "Toggle wikitext view" :: Text
  let modeJson = "Toggle JSON view" :: Text
  let modeHtml = "Toggle HTML view" :: Text
  let modeRender = "Toggle preview" :: Text
  let modeHistory = "Toggle transaction history view" :: Text
  let modeHelp = "Toggle help view" :: Text

  defaultLayout $ do
    addStylesheetRemote "/static/visual-editor/modules/ve/es/styles/ve.es.Document.css"
    addStylesheetRemote "/static/visual-editor/modules/ve/es/styles/ve.es.Content.css"
    addStylesheetRemote "/static/visual-editor/modules/ve/es/styles/ve.es.Surface.css"
    addStylesheetRemote "/static/visual-editor/modules/ve/ui/styles/ve.ui.Context.css"
    addStylesheetRemote "/static/visual-editor/modules/ve/ui/styles/ve.ui.Inspector.css"
    addStylesheetRemote "/static/visual-editor/modules/ve/ui/styles/ve.ui.Menu.css"
    addStylesheetRemote "/static/visual-editor/modules/ve/ui/styles/ve.ui.Toolbar.css"
    addStylesheetRemote "/static/visual-editor/modules/sandbox/sandbox.css"
    
    addScriptRemote "/static/visual-editor/modules/jquery/jquery.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.Position.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.Range.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.EventEmitter.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.Node.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.BranchNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/ve.LeafNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.Node.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.BranchNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.LeafNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.TransactionProcessor.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.Transaction.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/ve.dm.Surface.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.DocumentNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.HeadingNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.ParagraphNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.PreNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.ListItemNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.ListNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.TableCellNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.TableNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/nodes/ve.dm.TableRowNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/serializers/ve.dm.AnnotationSerializer.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/serializers/ve.dm.HtmlSerializer.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/serializers/ve.dm.JsonSerializer.js"
    addScriptRemote "/static/visual-editor/modules/ve/dm/serializers/ve.dm.WikitextSerializer.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/ve.es.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/ve.es.Node.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/ve.es.BranchNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/ve.es.LeafNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/ve.es.Content.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/ve.es.Surface.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.DocumentNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.HeadingNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.ParagraphNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.PreNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.ListItemNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.ListNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.TableCellNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.TableNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/es/nodes/ve.es.TableRowNode.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/ve.ui.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/ve.ui.Inspector.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/ve.ui.Tool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/ve.ui.Toolbar.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/ve.ui.Context.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/ve.ui.Menu.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/inspectors/ve.ui.LinkInspector.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.ButtonTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.AnnotationButtonTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.ClearButtonTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.HistoryButtonTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.ListButtonTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.IndentationButtonTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.DropdownTool.js"
    addScriptRemote "/static/visual-editor/modules/ve/ui/tools/ve.ui.FormatDropdownTool.js"
    
    setTitle "Document"
    $(widgetFile "document")

getTransactionsChan :: DocumentId -> Handler (Chan ServerEvent)
getTransactionsChan docid = do
  -- TODO: rethink increment of numClient counter
  y <- getYesod
  liftIO $ modifyMVar (documentsMap y) $ \docmap ->
    case M.lookup docid docmap of
      Nothing -> do
        let numClients = 1
        chan <- newChan
        let docmap' = M.insert docid (numClients, chan) docmap
        return $ (docmap', chan)
      Just (numClients, chan) -> do
        let numClients' = numClients + 1
        let docmap' = M.insert docid (numClients', chan) docmap
        return (docmap', chan)

getDocumentTransactionsR :: DocumentId -> Handler ()
getDocumentTransactionsR docid = do
  -- TODO: authorization
  chan <- getTransactionsChan docid
  chanClone <- liftIO $ dupChan chan
  req <- waiRequest
  res <- lift $ eventSourceApp chanClone req
  sendWaiResponse res

postDocumentTransactionsR :: DocumentId -> Handler ()
postDocumentTransactionsR docid = do
  -- TODO: authorization
  chan <- getTransactionsChan docid
  req <- waiRequest
  json <- fmap BS.concat $ liftIO $ runResourceT $ requestBody req $$ CL.consume
  liftIO $ print json
  liftIO $ writeChan chan $ ServerEvent Nothing Nothing [fromByteString json]
  sendResponseCreated $ DocumentR docid