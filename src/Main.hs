{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson                 (Value)
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import           Data.Byteable
import           Data.ByteString.Base64
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
    time <- getTime
    print time

    accounts <- listAccounts
    CLBS.putStrLn $ encodePretty accounts

-- listAccounts - Example of making an authenticated request against GDAX.

listAccounts :: (MonadIO m, MonadThrow m) => m Value
listAccounts = do
    accessKey <- liftIO $ CBS.pack <$> getEnv "GDAX_KEY"
    secretKey <- liftIO $ decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    passphrase <- liftIO $ CBS.pack <$> getEnv "GDAX_PASSPHRASE"
    time <- liftIO getCurrentTime

    let timestamp = CBS.pack $ printf "%.0f" (realToFrac (utcTimeToPOSIXSeconds time) :: Double)
        sigString = timestamp <> "GET" <> "/accounts"
        sig = encode $ toBytes (hmac secretKey sigString :: HMAC SHA256)
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
