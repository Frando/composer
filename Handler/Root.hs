module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = do
  defaultLayout $ do
    setTitle "Composer homepage"
    $(widgetFile "homepage")
