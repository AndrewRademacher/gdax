{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson.Lens
import           Data.Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Wreq

main :: IO ()
main = do
    time <- getTime
    print time

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
