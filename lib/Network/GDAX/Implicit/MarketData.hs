{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Implicit.MarketData where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Time
import           Network.GDAX.Core
import qualified Network.GDAX.Explicit.MarketData as Explicit

getTime :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m UTCTime
getTime = do
    g <- (^. gdax) <$> ask
    Explicit.getTime g
