module Demo.Server where

import Prelude

import Conduit (ResourceT)

import Database.Persist
import Database.Persist.Sqlite

import Servant
import Servant.API

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Reader

import Data.Aeson (FromJSON(..), (.:), (.:?), ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Data.List (nub)

import Data.Proxy (Proxy)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString (toStrict)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.ByteString.Base64 (encodeBase64)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP (newTlsManager)

import Data.Time

import Demo.Image
import Demo.Service

import Google.Cloud.Vision

import Crypto.Hash (hash, Digest, MD5)

--
-- API
--

type DemoAPI
    =    GetImageEndpoint
    -- Redundant and forwarded to by SearchImagesEndpoint
    -- :<|> GetAllImagesEndpoint
    :<|> SearchImagesEndpoint
    :<|> SubmitImageEndpoint

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

demoServerT :: ServerT DemoAPI ServiceM
demoServerT
    =    getImageHandler
    -- Redundant and forwarded to by SearchImagesEndpoint
    -- :<|> getAllImagesHandler
    :<|> searchImagesHandler
    :<|> submitImageHandler

demoServer :: Service -> Server DemoAPI
demoServer service = hoistServer demoAPI (flip runServiceT service) demoServerT

demoApp :: Service -> Application
demoApp service = serve demoAPI (demoServer service)

--
-- DB
--

-- NOTE: This runs the migration every query, which is not the best idea
--  We should run migration once at startup.
runDB :: (MonadUnliftIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB a = runSqlite "images.db" $ runMigration migrateAll >> a

--
-- Endpoints
--

-- GetImage

newtype GetImageResponse = GetImageResponse
    { getImage :: ImageMetadata
    }
    deriving newtype (FromJSON, ToJSON)

type GetImageEndpoint
    = "images" :> Capture "imageId" ImageId :> Get '[JSON] GetImageResponse

getImageHandler :: ImageId -> ServiceM GetImageResponse
getImageHandler imageId = do
    mimage <- liftIO . runDB $ selectFirst [ImageMetadataImageId ==. imageId] [LimitTo 1]
    case mimage of
        Nothing     -> fail "Image does not exist" -- TODO: 404 here
        Just image  -> return $ GetImageResponse (entityVal image)
 
-- GetAllImages

newtype GetAllImagesResponse = GetAllImagesResponse
    { getAllImages :: [ImageMetadata]
    }
    deriving newtype (FromJSON, ToJSON)

type GetAllImagesEndpoint
    = "images" :> Get '[JSON] GetAllImagesResponse

getAllImagesHandler :: ServiceM GetAllImagesResponse
getAllImagesHandler = do
    images <- liftIO . runDB $ selectList [] []
    return $ GetAllImagesResponse (entityVal <$> images)

-- SearchImages

newtype SearchImagesResponse = SearchImagesResponse
    { getSearchResults :: [ImageMetadata]
    }
    deriving newtype (FromJSON, ToJSON)

type SearchImagesEndpoint
    = "images" :> QueryParam "objects" ImageObjects :> Get '[JSON] SearchImagesResponse

-- NOTE: This occludes getAllImagesHandler
searchImagesHandler :: Maybe ImageObjects -> ServiceM SearchImagesResponse
searchImagesHandler Nothing        = forwardGetAllImagesHandler
searchImagesHandler (Just objects) = case objects of
    ImageObjects []     -> return $ SearchImagesResponse []
    ImageObjects objs   -> do
        -- NOTE: Use of toLower to flatten search to lowercase
        let filter = foldl (\x y -> x ||. [objectsLike $ Text.toLower y]) [] $ objs
        images <- liftIO . runDB $ selectList filter []
        return $ SearchImagesResponse (entityVal <$> images)

-- NOTE: This took a bit of digging to figure out tell Persistent I needed a LIKE statement
objectsLike :: Text -> Filter ImageMetadata
objectsLike arg = Filter
    ImageMetadataObjects
    (UnsafeValue $ "%" <> arg <> "%")
    (BackendSpecificFilter "LIKE")

forwardGetAllImagesHandler :: ServiceM SearchImagesResponse
forwardGetAllImagesHandler = (SearchImagesResponse . getAllImages) <$> getAllImagesHandler

-- SubmitImage

data SubmitImageRequest = SubmitImageRequest
    { submitImageRequestImageSource :: ImageSource
    , submitImageRequestImageLabel :: Maybe Text
    , submitImageRequestDetectObjects :: Bool
    }

instance FromJSON SubmitImageRequest where

    parseJSON :: Aeson.Value -> Aeson.Parser SubmitImageRequest
    parseJSON = Aeson.withObject "SubmitImageRequest" $ \ v -> SubmitImageRequest
        <$> v .:  "image"
        <*> v .:? "label"
        <*> v .:  "detectObjects"

instance ToJSON SubmitImageRequest where

    toJSON :: SubmitImageRequest -> Aeson.Value
    toJSON x = Aeson.object
        [ "image"         .= submitImageRequestImageSource x
        , "label"         .= submitImageRequestImageLabel x
        , "detectObjects" .= submitImageRequestDetectObjects x
        ]
    
newtype SubmitImageResponse = SubmitImageResponse
    { getSubmitResults :: ImageMetadata
    }
    deriving newtype (FromJSON, ToJSON)

type SubmitImageEndpoint
    = "images" :> ReqBody '[JSON] SubmitImageRequest :> Post '[JSON] SubmitImageResponse

submitImageHandler :: SubmitImageRequest -> ServiceM SubmitImageResponse    
submitImageHandler req = convertResp where
    source        = submitImageRequestImageSource req
    mlabel        = submitImageRequestImageLabel req
    -- NOTE: This hashing function should be extracted
    imageId       = Text.pack . show $ case source of
        ImageSource  imageUri     -> hash $ Text.encodeUtf8 imageUri :: Digest MD5
        ImageContent base64String -> hash $ Text.encodeUtf8 base64String :: Digest MD5
    label         = maybe imageId id mlabel
    detectObjects = submitImageRequestDetectObjects req
    mkImage = ImageMetadata imageId label detectObjects . ImageObjects
    image = mkImage []
    -- NOTE: This converts a IMAGESOURCE request to a IMAGECONTENT request
    --  by downloading the image and converting it to base64
    convertResp = case source of
        ImageSource imageUri -> do
            manager <- liftIO $ HTTP.newTlsManager
            request <- liftIO $ HTTP.parseRequest $ Text.unpack imageUri
            response <- liftIO $ HTTP.httpLbs request manager
            submitImageHandler $ req
                { submitImageRequestImageSource = ImageContent
                    (encodeBase64 . ByteString.toStrict $ HTTP.responseBody response)
                }
        ImageContent _ -> resp
    resp = do
        -- Check to see if its already in the db
        existing <- liftIO . runDB $ selectFirst [ImageMetadataImageId ==. imageId] [LimitTo 1]
        case entityVal <$> existing of
            Nothing    -> if detectObjects
                then do
                    -- Analyze and insert image
                    detectedObjects <- analyze
                    let detectedImage = mkImage detectedObjects
                    liftIO . runDB $ insert detectedImage
                    return $ SubmitImageResponse detectedImage
                else do
                    -- Insert new unanalyzed image
                    liftIO . runDB $ insert image
                    return $ SubmitImageResponse image
            Just image -> if detectObjects && not (imageMetadataScanned image)
                then do
                    -- Analyze and update image
                    detectedObjects <- analyze
                    liftIO . runDB $ updateWhere
                        [ ImageMetadataImageId ==. imageId ]
                        [ ImageMetadataObjects =. ImageObjects detectedObjects
                        , ImageMetadataScanned =. True
                        ]
                    return $ SubmitImageResponse $ image
                        { imageMetadataObjects = ImageObjects detectedObjects
                        , imageMetadataScanned = True
                        }
                else do
                    -- Do nothing and return the image
                    return $ SubmitImageResponse image
    -- This requests and unpacks the google vision response. The long names make it unreadable.
    analyze = do
        clientEnv <- asks serviceGoogleCloudVisionAPIClientEnv
        apiKey <- asks serviceGoogleCloudVisionAPIKey
        analysis <- mkVisionClient clientEnv (Just apiKey) $ VisionImagesAnnotationRequest
            { visionImagesAnnotationRequestRequests =
                [ VisionImageAnnotationRequest
                    { visionImageAnnotationRequestImage = case source of
                        ImageSource imageUri -> VisionImageAnnotationRequestImage
                            Nothing
                            (Just $ VisionImageAnnotationRequestImageSource imageUri)
                        ImageContent base64String -> VisionImageAnnotationRequestImage
                            (Just base64String)
                            Nothing
                    , visionImageAnnotationRequestFeatures =
                        [ VisionImageAnnotationRequestFeature
                            { visionImageAnnotationRequestFeatureType = VisionImageAnnotationRequestFeatureTypeObjectLocalization
                            , visionImageAnnotationRequestFeatureMaxResults = 10
                            , visionImageAnnotationRequestFeatureModel = VisionImageAnnotationRequestFeatureModelBuiltinStable
                            }
                        ]
                    }
                ]
            }
        -- NOTE: If we get this far, this response must exist according to the Google API.
        --  Head is still unsafe if they for instance failed to return a result in their response.
        -- NOTE: The use of nub to prune duplicate image responses - in the google vision API,
        --  if an image contains two fishes, it will return two hits for fish, with locations.
        --  This extra information is discarded and we only keep which hit names there were
        -- NOTE: Use of toLower to flatten search to lowercase
        let result = visionImageAnnotationResponseLocalizedObjectAnnotations $ head $ visionImagesAnnotationResponseResponses analysis
        return $ fmap Text.toLower . nub $ visionImageAnnotationResponseLocalizedObjectAnnotationName <$> result
