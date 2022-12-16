module Main where

import Prelude

import Data.Function

import qualified Data.Text as Text

import Network.HTTP.Client.TLS (newTlsManager)

import Servant.Client

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger

import Demo.Server
import Demo.Service

service :: Service
service = defaultService
    -- { serviceHost = "localhost"
    -- , servicePort = 8081
    -- }

main :: IO ()
main = do
    manager <- newTlsManager
    let 
        client = mkClientEnv manager (BaseUrl Https "vision.googleapis.com" 443 "")
        service = defaultService
            { serviceGoogleCloudVisionAPIClientEnv = client
            , serviceGoogleCloudVisionAPIKey = "your-google-cloud-vision-api"
            }
    putStrLn $ "Starting server @ " ++ Text.unpack (serviceHost service) ++ ":" ++ show (servicePort service)
    withStdoutLogger $ \ logger -> do
        flip runSettings (demoApp service) $ defaultSettings
            & setPort (servicePort service)
            & setLogger logger
