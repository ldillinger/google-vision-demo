module Google.Cloud.Vision where

import Prelude

import Servant.Client
import Servant.API

import Control.Monad.IO.Unlift

import Data.Function
import Data.Proxy

import Data.Aeson (FromJSON(..), (.:), (.:?), ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Text (Text)

import Demo.Image
import Demo.Service

data VisionImagesAnnotationRequest = VisionImagesAnnotationRequest
    { visionImagesAnnotationRequestRequests :: [VisionImageAnnotationRequest]
    }
    
instance ToJSON VisionImagesAnnotationRequest where

    toJSON :: VisionImagesAnnotationRequest -> Aeson.Value
    toJSON x = Aeson.object
        [ "requests" .= visionImagesAnnotationRequestRequests x
        ]

data VisionImageAnnotationRequest = VisionImageAnnotationRequest
    { visionImageAnnotationRequestImage :: VisionImageAnnotationRequestImage
    , visionImageAnnotationRequestFeatures :: [VisionImageAnnotationRequestFeature]
    }

instance ToJSON VisionImageAnnotationRequest where

    toJSON :: VisionImageAnnotationRequest -> Aeson.Value
    toJSON x = Aeson.object
        [ "image" .= visionImageAnnotationRequestImage x
        , "features" .= visionImageAnnotationRequestFeatures x
        ]

data VisionImageAnnotationRequestImage = VisionImageAnnotationRequestImage
    { visionImageAnnotationRequestImageContent :: Maybe VisionImageAnnotationRequestImageContent
    , visionImageAnnotationRequestImageSource :: Maybe VisionImageAnnotationRequestImageSource
    }

instance ToJSON VisionImageAnnotationRequestImage where

    toJSON :: VisionImageAnnotationRequestImage -> Aeson.Value
    toJSON x = Aeson.object
        [ "content" .= visionImageAnnotationRequestImageContent x
        , "source" .= visionImageAnnotationRequestImageSource x
        ]

-- NOTE: Base64-encoded data string
type VisionImageAnnotationRequestImageContent = Text

data VisionImageAnnotationRequestImageSource = VisionImageAnnotationRequestImageSource
    { visionImageAnnotationRequestImageSourceImageUri :: Text
    }

instance ToJSON VisionImageAnnotationRequestImageSource where

    toJSON :: VisionImageAnnotationRequestImageSource -> Aeson.Value
    toJSON x = Aeson.object
        [ "imageUri" .= visionImageAnnotationRequestImageSourceImageUri x
        ]

data VisionImageAnnotationRequestFeature = VisionImageAnnotationRequestFeature
    { visionImageAnnotationRequestFeatureType :: VisionImageAnnotationRequestFeatureType
    , visionImageAnnotationRequestFeatureMaxResults :: Int
    , visionImageAnnotationRequestFeatureModel :: VisionImageAnnotationRequestFeatureModel
    }

instance ToJSON VisionImageAnnotationRequestFeature where

    toJSON :: VisionImageAnnotationRequestFeature -> Aeson.Value
    toJSON x = Aeson.object
        [ "type" .= visionImageAnnotationRequestFeatureType x
        , "maxResults" .= visionImageAnnotationRequestFeatureMaxResults x
        , "model" .= visionImageAnnotationRequestFeatureModel x
        ]

data VisionImageAnnotationRequestFeatureType
    = VisionImageAnnotationRequestFeatureTypeObjectLocalization

instance ToJSON VisionImageAnnotationRequestFeatureType where

    toJSON :: VisionImageAnnotationRequestFeatureType -> Aeson.Value
    toJSON VisionImageAnnotationRequestFeatureTypeObjectLocalization = Aeson.String "OBJECT_LOCALIZATION"

data VisionImageAnnotationRequestFeatureModel
    = VisionImageAnnotationRequestFeatureModelBuiltinStable

instance ToJSON VisionImageAnnotationRequestFeatureModel where

    toJSON :: VisionImageAnnotationRequestFeatureModel -> Aeson.Value
    toJSON VisionImageAnnotationRequestFeatureModelBuiltinStable = Aeson.String "builtin/stable"

data VisionImagesAnnotationResponse = VisionImagesAnnotationResponse
    { visionImagesAnnotationResponseResponses :: [VisionImageAnnotationResponse]
    }
    
instance FromJSON VisionImagesAnnotationResponse where

    parseJSON :: Aeson.Value -> Aeson.Parser VisionImagesAnnotationResponse
    parseJSON = Aeson.withObject "VisionImagesAnnotationResponse" $ \ v -> VisionImagesAnnotationResponse
        <$> v .: "responses"

data VisionImageAnnotationResponse = VisionImageAnnotationResponse
    { visionImageAnnotationResponseLocalizedObjectAnnotations :: [VisionImageAnnotationResponseLocalizedObjectAnnotation]
    }
    
instance FromJSON VisionImageAnnotationResponse where

    parseJSON :: Aeson.Value -> Aeson.Parser VisionImageAnnotationResponse
    parseJSON = Aeson.withObject "VisionImageAnnotationResponse" $ \ v -> VisionImageAnnotationResponse
        <$> v .: "localizedObjectAnnotations"

data VisionImageAnnotationResponseLocalizedObjectAnnotation = VisionImageAnnotationResponseLocalizedObjectAnnotation
    { visionImageAnnotationResponseLocalizedObjectAnnotationMid :: Text
    , visionImageAnnotationResponseLocalizedObjectAnnotationName :: Text
    , visionImageAnnotationResponseLocalizedObjectAnnotationScore :: Double
    -- , visionImageAnnotationResponseLocalizedObjectAnnotationBoundingPoly :: ...
    }

instance FromJSON VisionImageAnnotationResponseLocalizedObjectAnnotation where

    parseJSON :: Aeson.Value -> Aeson.Parser VisionImageAnnotationResponseLocalizedObjectAnnotation
    parseJSON = Aeson.withObject "VisionImageAnnotationResponseLocalizedObjectAnnotation" $ \ v -> VisionImageAnnotationResponseLocalizedObjectAnnotation
        <$> v .: "mid"      
        <*> v .: "name"    
        <*> v .: "score"

type VisionAPI
    = "v1" :> "images:annotate"
    :> QueryParam "key" GoogleCloudVisionAPIKey
    :> ReqBody '[JSON] VisionImagesAnnotationRequest
    :> Post '[JSON] VisionImagesAnnotationResponse

visionAPI :: Proxy VisionAPI
visionAPI = Proxy

visionClientM :: Client ClientM VisionAPI
visionClientM = client visionAPI

mkVisionClient
    :: ClientEnv
    -> Maybe GoogleCloudVisionAPIKey
    -> VisionImagesAnnotationRequest
    -> ServiceM VisionImagesAnnotationResponse
mkVisionClient env = hoistClient visionAPI clientMToServiceM visionClientM where
    eitherToError = (either (error . show) id)
    clientMToServiceM = fmap eitherToError . liftIO . flip runClientM env
