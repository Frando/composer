module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = do
  defaultLayout $ do
    setTitle "substantial homepage"
    $(widgetFile "homepage")
