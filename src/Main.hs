{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens               hiding ((.=))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson                 (Value, object, (.=))
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import           Data.Byteable
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Wreq
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
    -- putStrLn "getTime:"
    -- time <- getTime
    -- print time

    putStrLn "listAccounts:"
    accounts <- listAccounts
    CLBS.putStrLn $ encodePretty accounts

    putStrLn "placeOrder:"
    ores <- placeOrder
    CLBS.putStrLn $ encodePretty ores

-- placeOrder - Example of making authenticated request with body against GDAX.

placeOrder :: (MonadIO m, MonadThrow m) => m Value
placeOrder = do
    -- Sig stuff
    accessKey <- liftIO $ CBS.pack <$> getEnv "GDAX_KEY"
    secretKey <- liftIO $ Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    passphrase <- liftIO $ CBS.pack <$> getEnv "GDAX_PASSPHRASE"
    time <- liftIO getCurrentTime

    -- Body stuff
    let body = object
            [ "size" .= ("0.01" :: Text)
            , "price" .= ("0.100" :: Text)
            , "side" .= ("buy" :: Text)
            , "product_id" .= ("BTC-USD" :: Text)
            ]
        bodyBS = CLBS.toStrict $ Aeson.encode body

    liftIO $ print $ bodyBS

    -- Sig stuff
    let timestamp = CBS.pack $ printf "%.0f" (realToFrac (utcTimeToPOSIXSeconds time) :: Double)
        sigString :: ByteString
        sigString = timestamp <> "POST" <> "/orders" <> bodyBS
        sig = Base64.encode $ toBytes (hmac secretKey sigString :: HMAC SHA256)
        opts = defaults
            & header "CB-ACCESS-KEY" .~ [ accessKey ]
            & header "CB-ACCESS-SIGN" .~ [ sig ]
            & header "CB-ACCESS-TIMESTAMP" .~ [ timestamp ]
            & header "CB-ACCESS-PASSPHRASE" .~ [ passphrase ]
            & header "Content-Type" .~ [ "application/json" ]

    -- res <- liftIO $ postWith opts "https://api.gdax.com/orders" bodyBS
    res <- liftIO $ postWith opts "https://api-public.sandbox.gdax.com/orders" bodyBS
    case res ^? responseBody . _Value of
        Nothing -> throwM $ MalformedGDAXResponse "Order post response body was invalid."
        Just val -> return val

-- listAccounts - Example of making an authenticated request against GDAX.

listAccounts :: (MonadIO m, MonadThrow m) => m Value
listAccounts = do
    accessKey <- liftIO $ CBS.pack <$> getEnv "GDAX_KEY"
    secretKey <- liftIO $ Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    passphrase <- liftIO $ CBS.pack <$> getEnv "GDAX_PASSPHRASE"
    time <- liftIO getCurrentTime

    let timestamp = CBS.pack $ printf "%.0f" (realToFrac (utcTimeToPOSIXSeconds time) :: Double)
        sigString = timestamp <> "GET" <> "/accounts"
        sig = Base64.encode $ toBytes (hmac secretKey sigString :: HMAC SHA256)
        opts = defaults
                & header "CB-ACCESS-KEY" .~ [ accessKey ]
                & header "CB-ACCESS-SIGN" .~ [ sig ]
                & header "CB-ACCESS-TIMESTAMP" .~ [ timestamp ]
                & header "CB-ACCESS-PASSPHRASE" .~ [ passphrase ]

    res <- liftIO $ getWith opts "https://api.gdax.com/accounts"
    case res ^? responseBody . _Value of
        Nothing -> throwM $ MalformedGDAXResponse "Account response body was invalid."
        Just val -> return val

-- getTime - Example of making an unauthenticated request against GDAX.

getTime :: (MonadIO m, MonadThrow m) => m UTCTime
getTime = do
    res <- liftIO $ get "https://api.gdax.com/time"
    case res ^? responseBody . key "epoch" . _Double of
        Nothing  -> throwM $ MalformedGDAXResponse "Epoch field was either missing or malformed in response from GET /time."
        Just val -> return $ posixSecondsToUTCTime $ realToFrac val

data MalformedGDAXResponse
    = MalformedGDAXResponse Text
    deriving (Show)

instance Exception MalformedGDAXResponse
