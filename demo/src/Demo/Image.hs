{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Demo.Image where

import Prelude

import Control.Applicative

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Servant.API

import Data.Aeson (FromJSON(..), (.:), (.:?), ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Proxy (Proxy(..))

import Data.Text (Text)
import qualified Data.Text as Text

--
-- Image identifier
--

-- NOTE: Need Web.PathPieces.PathPiece conformance...
{-
newtype ImageId = ImageId
    { getImageId :: Text
    }
    deriving newtype
        ( Show
        , Read
        , Eq
        , Ord
        , FromJSON
        , ToJSON
        , PersistField
        , PersistFieldSql
        )

instance FromHttpApiData ImageId where

    parseQueryParam :: Text -> Either Text ImageId
    parseQueryParam = Right . ImageId

instance ToHttpApiData ImageId where

    toQueryParam :: ImageId -> Text
    toQueryParam = getImageId
-}
-- Or, for now, even though we prefer newtypes:
type ImageId = Text
    
--
-- Image objects
--

newtype ImageObjects = ImageObjects
    { getImageObjects :: [Text]
    }
    deriving newtype
        ( Show
        , FromJSON
        , ToJSON
        )

imageObjectsToCSVText :: ImageObjects -> Text
imageObjectsToCSVText = Text.intercalate "," . getImageObjects

csvTextToImageObjects :: Text -> ImageObjects
csvTextToImageObjects = ImageObjects . Text.splitOn ","


instance FromHttpApiData ImageObjects where

    parseQueryParam :: Text -> Either Text ImageObjects
    parseQueryParam = Right . csvTextToImageObjects

instance PersistField ImageObjects where

    toPersistValue :: ImageObjects -> PersistValue
    toPersistValue = PersistText . imageObjectsToCSVText
    
    fromPersistValue :: PersistValue -> Either Text ImageObjects
    fromPersistValue (PersistText text) = Right $ csvTextToImageObjects text
    fromPersistValue _ = Left "Expected PersistText"

instance PersistFieldSql ImageObjects where

    sqlType :: Proxy ImageObjects -> SqlType
    sqlType _ = SqlString

--
-- Image metadata
--

{-
data ImageMetadata = ImageMetadata
    { imageMetadataImageId :: ImageId
    , imageMetadataLabel :: Text
    , imageMetadataScanned :: Bool
    , imageMetadataObjects :: ImageObjects
    }
-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ImageMetadata
    imageId ImageId
    label Text
    scanned Bool
    objects ImageObjects
    deriving Show

    Primary imageId
|]

instance FromJSON ImageMetadata where

    parseJSON :: Aeson.Value -> Aeson.Parser ImageMetadata
    parseJSON = Aeson.withObject "ImageMetadata" $ \ v -> ImageMetadata
        <$> v .: "id"
        <*> v .: "label"
        <*> v .: "scanned"
        <*> v .: "objects"

instance ToJSON ImageMetadata where

    toJSON :: ImageMetadata -> Aeson.Value
    toJSON x = Aeson.object
        [ "id"      .= imageMetadataImageId x
        , "label"   .= imageMetadataLabel x
        , "scanned" .= imageMetadataScanned x
        , "objects" .= imageMetadataObjects x
        ]

--
-- Image Source
--

-- NOTE: This is not exactly the same as the Image in Google.Cloud.Vision
data ImageSource
    = ImageContent Base64String
    | ImageSource ImageURI

type Base64String = Text
type ImageURI = Text

instance FromJSON ImageSource where

    -- NOTE: This will prefer content over source if both are present
    -- NOTE: This should be grossly simplifiable with applicatives / monads
    --  but the inlined version is fine for now
    parseJSON :: Aeson.Value -> Aeson.Parser ImageSource
    parseJSON = Aeson.withObject "ImageSource" $ \ v -> do
        c <- v .:? "content"
        case c of
            Nothing -> do
                s <- v .:? "source"
                case s of
                    Nothing -> fail "Expected content or source"
                    Just s  -> pure (ImageSource s)
            Just c  -> pure (ImageContent c)

instance ToJSON ImageSource where

    toJSON :: ImageSource -> Aeson.Value
    toJSON (ImageContent x) = Aeson.object
        [ "content" .= x
        ]
    toJSON (ImageSource x) = Aeson.object
        [ "source" .= x
        ]

--
-- Image
--

data Image = Image
    { imageSource :: ImageSource
    , imageMetadata :: ImageMetadata
    }