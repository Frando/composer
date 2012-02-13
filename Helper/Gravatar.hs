module Helper.Gravatar
  ( gravatar
  , maybeGravatar
  ) where

import Prelude
import Data.Digest.Pure.MD5 (md5)
import Data.Char (isSpace, toLower)
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.Text as T

-- (c) Michael Snoyman
-- https://github.com/snoyberg/haskellers/blob/master/Handler/Root.hs
gravatar :: Int -> T.Text -> T.Text
gravatar s x = T.concat
  [ "http://www.gravatar.com/avatar/"
  , hash
  , "?d=identicon&s="
  , T.pack $ show s
  ]
  where
    hash = T.pack $ show $ md5 $ L.fromString $ map toLower $ trim $ T.unpack x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

maybeGravatar :: Int -> Maybe T.Text -> T.Text
maybeGravatar size memail = gravatar size $ maybe "example@example.com" id memail