module Model.Transaction
  ( Operation (..)
  , Transaction (..)
  ) where

import Prelude
--import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Control.Applicative ((<$>))
import Control.Monad (mzero)

data Annotation = Annotation

data VEChar = VEChar Char [Annotation]
type VEText = [VEChar]

data Operation = Retain Int
               | Insert VEText
               | Delete VEText

data Transaction = Transaction Int [Operation]

instance ToJSON Annotation where
  toJSON Annotation = Null

instance FromJSON Annotation where
  parseJSON _ = return Annotation

instance ToJSON VEChar where
  toJSON (VEChar ch []) = toJSON ch
  toJSON (VEChar ch as) = toJSON $ (toJSON ch):(map toJSON as)

instance FromJSON VEChar where
  parseJSON (Array a) | not (V.null a) = parseJSON $ V.head a
  parseJSON s = (\ch -> VEChar ch []) <$> parseJSON s

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

instance FromJSON Operation where
  parseJSON (Object o) = do
    type' <- o .: "type"
    case type' of
      String "retain" -> Retain <$> o .: "length"
      String "insert" -> Insert <$> o .: "data"
      String "remove" -> Delete <$> o .: "data"
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