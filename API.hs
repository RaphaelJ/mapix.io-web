{-# LANGUAGE OverloadedStrings #-}

module API (
      APIImageCode (..), Tag, HasAPIConfig (..)
    -- * API Calls
    , postObjects
    -- * Utils
    , getResourceUrl, getOptions, withMashapeKey
    ) where

import Prelude

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (String), encode)
import Data.Aeson.Lens (key)
import Data.ByteString (ByteString)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql (..))
import Network.HTTP.Client.Conduit (HasHttpManager (getHttpManager))
import Network.Wreq (
      Options
    , defaults, header, manager, partLBS, partFileSource, partText, partString
    , postWith, responseBody
    )

newtype APIImageCode = APIImageCode { aicValue :: Text }
    deriving (Eq, Ord, Read, PersistField)

instance PersistFieldSql APIImageCode where
    sqlType action = sqlType (aicValue `liftM` action)

type Tag = Text

class HasAPIConfig a where
    getApiRoot :: a -> String
    getApiKey  :: a -> ByteString

postObjects :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
               , MonadIO m)
            => Maybe Text -> FilePath -> [Tag] -> Bool -> m (Maybe Text)
postObjects mName path tags ignoreBack = do
    options <- getOptions
    url     <- getResourceUrl "/objects"

    let ignoreBackStr = if ignoreBack then "1" else "0"
        postArgs      = maybeToList (partText "name" <$> mName) ++
                        [ partFileSource "images"            path
                        , partLBS        "tags"              (encode tags)
                        , partString     "ignore_background" ignoreBackStr ]

    resp <- liftIO $ postWith options url postArgs

    return $ case resp ^? responseBody . key "id" of
        Just (String code) -> Just code
        _                  -> Nothing

-- -----------------------------------------------------------------------------

getResourceUrl :: (MonadReader env m, HasAPIConfig env) => String -> m String
getResourceUrl rsrc = ((++ rsrc) . getApiRoot) `liftM` ask

-- | Creates an 'Options' object from the monad's HTTP 'Manager' which can be
-- used to query the API.
getOptions :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
              , MonadIO m)
           => m Options
getOptions = do
    env <- ask

    let httpMan = getHttpManager env
        options = defaults & manager .~ Right httpMan

    withMashapeKey options

-- | Adds the X-Mashape-Key header with the Monad's API key to every request
-- which will be made with the returned 'Options' object.
withMashapeKey :: (HasAPIConfig env, MonadReader env m) => Options -> m Options
withMashapeKey options = do
    env <- ask

    let apiKey   = getApiKey env
        options' = options & header "X-Mashape-Key" .~ [apiKey]

    return options'
