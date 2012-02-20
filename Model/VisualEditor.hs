module Model.VisualEditor
  ( Annotation (..)
  , Element (..)
  , VEChar (..)
  , VEDocument (..)
  , VEOperation (..)
  , VETransaction (..)
  , applyTransaction
  ) where

import Prelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Control.Applicative ((<$>))
import Control.Monad (mzero)

data Annotation = Annotation
  { annotationType :: T.Text
  } deriving (Eq, Show, Read)

data Element = Paragraph
             deriving (Eq, Show, Read)

data VEChar = VEChar Char [Annotation]
            | StartTag Element
            | EndTag Element
            deriving (Eq, Show, Read)
type VEText = [VEChar]

data VEOperation = Retain Int
                 | Insert VEText
                 | Delete VEText
                 | StartAnnotation Annotation
                 | StopAnnotation Annotation
                 deriving (Eq, Show, Read)

data VETransaction = VETransaction Int [VEOperation] deriving (Eq, Show, Read)

newtype VEDocument = VEDocument [VEChar] deriving (Eq, Show, Read)

applyTransaction :: VEDocument -> VETransaction -> VEDocument
applyTransaction (VEDocument chars) (VETransaction _ operations) = VEDocument $ go operations chars
  where
    go :: [VEOperation] -> [VEChar] -> [VEChar]
    go [] [] = []
    go [] _  = error "unretained input"
    go (Retain 0 : ops) chs = go ops chs
    go (Retain n : ops) (ch:chs) = ch:(go (Retain (n-1) : ops) chs)
    go (Retain _ : _) [] = error "no input left to retain"
    go (Insert [] : ops) chs = go ops chs
    go (Insert (a:as) : ops) chs = a:(go (Insert as : ops) chs)
    go (Delete (a:as) : ops) (ch:chs) | a == ch = go (Delete as : ops) chs
                                      | otherwise = error "Char to delete and actual char don't match"
    -- TODO: other operations
    go (_:ops) chs = go ops chs

instance ToJSON Annotation where
  toJSON (Annotation type') = object
    [ "type" .= type'
    ]

instance FromJSON Annotation where
  parseJSON (Object o) = Annotation <$> o .: "type"
  parseJSON _ = mzero

elementToText :: Element -> T.Text
elementToText Paragraph = "paragraph"

elementFromText :: T.Text -> Maybe Element
elementFromText "paragraph" = Just Paragraph
elementFromText _ = Nothing

instance ToJSON VEChar where
  toJSON (VEChar ch []) = toJSON ch
  toJSON (VEChar ch as) = toJSON $ (toJSON ch):(map toJSON as)
  toJSON (StartTag e) = object
    [ "type" .= elementToText e
    ]
  toJSON (EndTag e) = object
    [ "type" .= T.cons '/' (elementToText e)
    ]

instance FromJSON VEChar where
  parseJSON (Array a) | not (V.null a) = do
    ch <- parseJSON $ V.head a
    as <- parseJSON $ Array (V.tail a)
    return $ VEChar ch as
  parseJSON (String s) = (\ch -> VEChar ch []) <$> parseJSON (String s)
  parseJSON (Object o) = do
    type' <- o .: "type"
    if T.null type'
      then mzero
      else do
        let isEndTag = T.head type' == '/'
        let elementText = if isEndTag then T.tail type' else type'
        case elementFromText elementText of
          Nothing -> mzero
          Just e  -> return $ (if isEndTag then EndTag else StartTag) e
  parseJSON _ = mzero

instance ToJSON VEOperation where
  toJSON (Retain n) = object
    [ "type" .= String "retain"
    , "length" .= n
    ]
  toJSON (Insert text) = object
    [ "type" .= String "insert"
    , "data" .= text
    ]
  toJSON (Delete text) = object
    [ "type" .= String "remove"
    , "data" .= text
    ]
  toJSON (StartAnnotation a) = object
    [ "type" .= String "annotate"
    , "bias" .= String "start"
    , "annotation" .= a
    ]
  toJSON (StopAnnotation a) = object
    [ "type" .= String "annotate"
    , "bias" .= String "stop"
    , "annotation" .= a
    ]

instance FromJSON VEOperation where
  parseJSON (Object o) = do
    type' <- o .: "type"
    case type' of
      String "retain" -> Retain <$> o .: "length"
      String "insert" -> Insert <$> o .: "data"
      String "remove" -> Delete <$> o .: "data"
      String "annotate" -> do
        bias <- o .: "bias"
        annotation <- o .: "annotation"
        case bias :: T.Text of
          "start" -> return $ StartAnnotation annotation
          "stop"  -> return $ StopAnnotation  annotation
          _       -> mzero
      _ -> mzero
  parseJSON _ = mzero

instance ToJSON VETransaction where
  toJSON (VETransaction sizediff ops) = object
    [ "lengthDifference" .= sizediff
    , "operations" .= ops
    ]

instance FromJSON VETransaction where
  parseJSON (Object o) = do
    lengthDifference <- o .: "lengthDifference"
    operations <- o .: "operations"
    return $ VETransaction lengthDifference operations 
  parseJSON _ = mzero

instance ToJSON VEDocument where
  toJSON (VEDocument chs) = toJSON chs

instance FromJSON VEDocument where
  parseJSON j = VEDocument <$> parseJSON j