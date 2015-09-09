module Model where

import Prelude

import Yesod
import Data.Text (Text)

import API (APIImageCode)

share [mkPersist sqlSettings, mkMigrate "migrateFlickr"] [persistLowerCase|
FlickrPicture
    flickrId                    Text
    apiId                       Text
    title                       Text
    owner                       FlickrOwnerId
    url                         Text
    tags                        [Text]

    UniqueFlickrImageFlickrId   flickrId
    UniqueFlickrImageApiId      apiId

    deriving Show

FlickrOwner
    flickrId                    Text
    name                        Text Maybe
    username                    Text Maybe

    UniqueFlickrOwnerFlickrId   flickrId

    deriving Show
|]
