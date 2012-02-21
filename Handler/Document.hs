module Handler.Document
  ( getNewDocumentR
  , postNewDocumentR
  , getDocumentR
  , deleteDocumentR
  , getDocumentSettingsR
  , postDocumentSettingsR
  , getDocumentTransactionsR
  , postDocumentTransactionsR
  ) where

import Import
import qualified Settings
import Data.Text (pack, unpack)
import Data.Time (getCurrentTime)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar_, modifyMVar, newMVar, readMVar)
import Control.Concurrent.Chan (newChan, dupChan, readChan, writeChan)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import qualified Data.Map as M
import Data.Aeson (Result (..), encode)
import Model.VisualEditor
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Database.Persist.Store (PersistConfig (..))

-- TODO: use existential quantification
type MyRunDB a = PersistConfigBackend Settings.PersistConfig IO a -> IO a

getMyRunDB :: Handler (MyRunDB a)
getMyRunDB = do
  y <- getYesod
  return $ \query -> runPool (persistConfig y) query (connPool y)

documentForm :: Document -> Html -> MForm Substantial Substantial (FormResult Document, Widget)
documentForm doc = renderTable $ Document
  <$> areq textField "Title" (Just $ documentTitle doc)
  <*> areq (radioFieldList publishSettings) "Publish" (Just $ documentPublishSettings doc)
  where
    publishSettings = map (pack . show &&& id) $ [minBound..maxBound]

defaultDocument :: Document
defaultDocument = Document "" PublishedVersionsOnly 

defaultContent :: VEDocument
defaultContent = VEDocument $ [StartTag Paragraph] ++ text ++ [EndTag Paragraph]
  where
    text = map (\ch -> VEChar ch []) "Hello World!"

getNewDocumentR :: Handler RepHtml
getNewDocumentR = do
  uid <- requireAuthId
  ((res, documentFormWidget), encoding) <- runFormPost $ documentForm defaultDocument
  case res of
    FormSuccess doc -> do
      now <- liftIO $ getCurrentTime
      docid <- runDB $ do
        docid <- insert doc
        _ <- insert $ Permission uid docid Author
        let firstVersion = Version docid False "" 0 now (pack . show $ defaultContent)
        _ <- insert firstVersion
        return docid
      redirect $ DocumentR docid
    _ -> return ()
  defaultLayout $ do
    setTitle "New Document"
    $(widgetFile "new-document")

postNewDocumentR :: Handler RepHtml
postNewDocumentR = getNewDocumentR

deleteDocumentR :: DocumentId -> Handler ()
deleteDocumentR docid = do
  runDB $ do
    deleteWhere [VersionDocument ==. docid]
    deleteWhere [TransactionDocument ==. docid]
    deleteWhere [PermissionDocument ==. docid]
    delete docid
  setMessage "Deleted document successfully!"
  redirect RootR

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

getDocumentSettingsR :: DocumentId -> Handler RepHtml
getDocumentSettingsR docid = do
  doc <- runDB $ get404 docid
  ((res, documentFormWidget), encoding) <- runFormPost $ documentForm doc
  doc' <- case res of
    FormSuccess doc' -> do
      runDB $ replace docid doc'
      setMessage "Updated settings."
      return doc'
    _ -> return doc
  defaultLayout $ do
    setTitle "Document settings"
    $(widgetFile "document-settings")

postDocumentSettingsR :: DocumentId -> Handler RepHtml
postDocumentSettingsR = getDocumentSettingsR

startDocumentThread :: MyRunDB () -> DocumentId -> Int -> VEDocument -> IO DocumentState
startDocumentThread myRunDB docid dbRevision initialVedoc = do
  doc <- newMVar initialVedoc
  inChan <- newChan
  outChan <- newChan
  let loop revision = do
      msg <- readChan inChan
      case msg of
        NewTransaction uid transaction -> do
          let revision' = revision + 1
          modifyMVar_ doc $ \vedoc -> do
            let vedoc' = applyTransaction vedoc transaction
            print vedoc'
            return vedoc'
          now <- getCurrentTime
          myRunDB $ do
            _ <- insert $ Transaction docid uid revision' now (pack $ show transaction)
            return ()
          writeChan outChan $ ServerEvent Nothing Nothing [fromLazyByteString $ encode transaction]
          loop $ revision'
  _ <- forkIO $ loop dbRevision
  return (doc, inChan, outChan)

getDocumentState :: DocumentId -> Handler DocumentState
getDocumentState docid = do
  y <- getYesod
  -- Yes, I know, that's not very elegant, but I couldn't get it working with
  -- ExistentialQuantification.
  myRunDB1 <- getMyRunDB
  myRunDB2 <- getMyRunDB
  myRunDB3 <- getMyRunDB
  liftIO $ modifyMVar (documentsMap y) $ \docmap ->
    case M.lookup docid docmap of
      Just state -> return (docmap, state)
      Nothing -> do
        mversion <- myRunDB1 $ selectFirst [VersionDocument ==. docid] [Desc VersionRevision]
        let version = entityVal . fromJust $ mversion
        let revision = versionRevision version
        let vedoc = read . unpack . versionContent $ version
        transactions <- myRunDB3 $ selectList [TransactionRevision >=. revision, TransactionDocument ==. docid] [Asc TransactionRevision]
        let vedoc' = applyTransactions vedoc $ map entityVal transactions
        state <- startDocumentThread myRunDB2 docid revision vedoc'
        let docmap' = M.insert docid state docmap
        return $ (docmap', state)
  where
    applyTransactions :: VEDocument -> [Transaction] -> VEDocument
    applyTransactions vedoc ts = foldl applyTransaction vedoc $ map (read . unpack . transactionChange) ts

getDocumentTransactionsR :: DocumentId -> Handler ()
getDocumentTransactionsR docid = do
  (vedocMVar, _, chan) <- getDocumentState docid
  chanClone <- liftIO $ dupChan chan
  vedoc <- liftIO $ readMVar vedocMVar
  req <- waiRequest
  res <- lift $ eventSourceApp chanClone req
  liftIO $ writeChan chan $ ServerEvent Nothing Nothing [fromLazyByteString $ encode vedoc]
  sendWaiResponse res

postDocumentTransactionsR :: DocumentId -> Handler ()
postDocumentTransactionsR docid = do
  uid <- requireAuthId
  (_, chan, _) <- getDocumentState docid
  transaction <- parseJsonBody :: Handler (Result VETransaction)
  liftIO $ print transaction
  case transaction of
    Error _ -> return ()
    Success t -> do
      liftIO $ writeChan chan $ NewTransaction uid t
  sendResponseCreated $ DocumentR docid