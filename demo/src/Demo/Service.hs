module Demo.Service where

import Prelude

import Servant (Handler, ServerError)
import Servant.Client (ClientEnv)

import Network.HTTP.Client (Manager)

import Control.Monad.Reader
-- import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as Text

import Demo.Image

-- Yes, this really should belong elsewhere
type GoogleCloudVisionAPIKey = Text

data Service = Service
    { serviceHost :: Text
    , servicePort :: Int
    , serviceGoogleCloudVisionAPIClientEnv :: ClientEnv
    , serviceGoogleCloudVisionAPIKey :: GoogleCloudVisionAPIKey
    }

-- NOTE: User MUST fill out GoogleCloudVisionAPIClientEnv, GoogleCloudVisionAPIKey
defaultService :: Service
defaultService = Service
    { serviceHost = "localhost"
    , servicePort = 8081
    , serviceGoogleCloudVisionAPIClientEnv = undefined
    , serviceGoogleCloudVisionAPIKey = undefined
    }

type ServiceM = ReaderT Service Handler

runServiceT :: ReaderT Service m a -> Service -> m a
runServiceT = runReaderT
