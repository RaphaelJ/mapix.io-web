module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "mapix.io - Index and retreive objects by their colors"
        $(widgetFile "homepage")
