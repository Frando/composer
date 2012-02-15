module Model.Transaction
  ( Operation (..)
  , Transaction (..)
  ) where

import Prelude
import Data.Text
import Data.Aeson

data Operation = Retain Int
               | Insert Text
               | Delete Text

instance ToJSON Operation where
  toJSON (Retain n) = object
    [ "type" .= String "retain"
    , "length" .= n
    ]
  toJSON (Insert text) = object
    [ "type" .= String "insert"
    , "data" .= [text]
    ]
  toJSON (Delete text) = object
    [ "type" .= String "remove"
    , "data" .= [text]
    ]

data Transaction = Transaction Int [Operation]

instance ToJSON Transaction where
  toJSON (Transaction sizediff ops) = object
    [ "lengthDifference" .= sizediff
    , "operations" .= ops
    ]