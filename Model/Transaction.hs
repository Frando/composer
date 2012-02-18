module Model.Transaction
  ( Operation (..)
  , Transaction (..)
  ) where

import Prelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Control.Applicative ((<$>))
import Control.Monad (mzero)

data Annotation = Annotation
  { annotationType :: T.Text
  } deriving (Eq, Show)

data Element = Paragraph
             deriving (Eq, Show)

data VEChar = VEChar Char [Annotation]
            | StartTag Element
            | EndTag Element
            deriving (Eq, Show)
type VEText = [VEChar]

data Operation = Retain Int
               | Insert VEText
               | Delete VEText
               | StartAnnotation Annotation
               | StopAnnotation Annotation
               deriving (Eq, Show)

data Transaction = Transaction Int [Operation] deriving (Eq, Show)

newtype VEDocument = VEDocument [VEChar]

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
          Just e  -> return $ (if isEndTag then StartTag else EndTag) e
  parseJSON _ = mzero

instance ToJSON Operation where
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

instance FromJSON Operation where
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

instance ToJSON Transaction where
  toJSON (Transaction sizediff ops) = object
    [ "lengthDifference" .= sizediff
    , "operations" .= ops
    ]

instance FromJSON Transaction where
  parseJSON (Object o) = do
    lengthDifference <- o .: "lengthDifference"
    operations <- o .: "operations"
    return $ Transaction lengthDifference operations 
  parseJSON _ = mzero

instance ToJSON VEDocument where
  toJSON (VEDocument chs) = toJSON chs

instance FromJSON VEDocument where
  parseJSON j = VEDocument <$> parseJSON j