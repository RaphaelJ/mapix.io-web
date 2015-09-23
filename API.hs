{-# LANGUAGE OverloadedStrings #-}

module API (
      APIImageCode (..), Tag, Weight, Color (..), HasAPIConfig (..)
    -- * API Calls
    , postObjects, searchByColors
    -- * Utils
    , getResourceUrl, getOptions, withMashapeKey
    ) where

import ClassyPrelude

import Control.Lens hiding ((.=))
import Data.Aeson (
      FromJSON (..), ToJSON (..), Value (Object, String)
    , (.=), (.:), encode, object
    )
import Data.Aeson.Lens (key)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql (..))
import Network.HTTP.Client.Conduit (HasHttpManager (getHttpManager))
import Network.Wreq (
      Options
    , asValue, defaults, header, manager, partLBS, partFileSource, partText
    , partString, postWith, responseBody
    )

newtype APIImageCode = APIImageCode { aicValue :: Text }
    deriving (Eq, Ord, Read, Show, PersistField)

instance PersistFieldSql APIImageCode where
    sqlType action = sqlType (aicValue `liftM` action)

type Tag = Text

type Weight = Double

-- | The color with its weight.
data Color = Color {
      cColor  :: (Word8, Word8, Word8) -- RGB
    , cWeight :: !Weight
    } deriving Show

instance ToJSON Color where
    toJSON (Color (r, g, b) w) =
        object [ "rgb"    .= [r, g, b]
               , "weight" .= w ]

instance FromJSON Color where
    parseJSON (Object o) = Color <$> o .: "rgb"
                                 <*> o .: "weight"
    parseJSON _          = mzero

class HasAPIConfig a where
    getApiRoot :: a -> String
    getApiKey  :: a -> ByteString

postObjects :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
               , MonadIO m)
            => Maybe Text -> FilePath -> [Tag] -> Bool -> m (Maybe APIImageCode)
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
        Just (String code) -> Just $ APIImageCode code
        _                  -> Nothing

searchByColors :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
                  , MonadIO m)
               => [Color] -> m Value
searchByColors colors = do
    options <- getOptions
    url     <- getResourceUrl "/search-by-colors"

    let postArgs = [ partLBS "colors" (encode colors) ]

    resp <- liftIO $ postWith options url postArgs >>= asValue

    return $! resp ^. responseBody

-- -----------------------------------------------------------------------------

-- | Prepends the API root to the given resource URI.
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
