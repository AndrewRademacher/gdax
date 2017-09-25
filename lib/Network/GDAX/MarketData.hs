{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.MarketData where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.GDAX.Core
import           Network.GDAX.Exceptions

-- getTime - Example of making an unauthenticated request against GDAX.

getTime' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m UTCTime
getTime' = do
    g <- (^. gdax) <$> ask
    getTime g

getTime :: (MonadIO m, MonadThrow m) => Gdax -> m UTCTime
getTime g = do
    res <- gdaxGet g "/time"
    case (res :: Value) ^? key "epoch" . _Double of
        Nothing  -> throwM $ MalformedGDAXResponse "Epoch field was either missing or malformed in response from GET /time."
        Just val -> return $ posixSecondsToUTCTime $ realToFrac val
