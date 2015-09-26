module Handler.Demo (
      getDemoR, postSearchByColorsR
    ) where

import Import

import Data.Aeson (decodeStrict', encode)

import API (searchByColors)

getDemoR :: Handler Html
getDemoR = do
    defaultLayout $ do
        setTitle "mapix.io - Search Flickr images by color"

        addStylesheet $! StaticR css_slider_css
        addScript     $! StaticR js_bootstrap_slider_js

        $(widgetFile "demo")

-- Search by colors ------------------------------------------------------------

postSearchByColorsR :: Handler Value
postSearchByColorsR = do
    colors <- runInputPost (ireq colorsField "colors")
    searchByColors colors
  where
    colorsField = jsonField "Invalid colors expression"

-- -----------------------------------------------------------------------------

-- | Accepts an error message and returns a field which decode the JSON text
-- field into the corresponding required type.
jsonField :: (RenderMessage (HandlerSite m) FormMessage, FromJSON a, ToJSON a
             , Monad m)
          => Text -> Field m a
jsonField err =
    let jsonParser txt =
            case decodeStrict' (encodeUtf8 txt) of Just node -> Right node
                                                   Nothing   -> Left err
    in checkMap jsonParser (decodeUtf8 . asByteString . repack . encode)
                textField

-- Same as 'check', but modifies the datatype.
--
-- In order to make this work, you must provide a function to convert back from
-- the new datatype to the old one (the second argument to this function).
checkMap :: Monad m
         => (a -> Either Text b) -> (b -> a) -> Field m a -> Field m b
checkMap f inv = checkMMap (return . f) inv
