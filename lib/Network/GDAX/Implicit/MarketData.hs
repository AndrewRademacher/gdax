{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Implicit.MarketData where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Time
import           Data.Vector
import           Network.GDAX.Core
import qualified Network.GDAX.Explicit.MarketData as Explicit
import           Network.GDAX.Types.MarketData

getProducts :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m (Vector Product)
getProducts = do
    g <- (^. gdax) <$> ask
    Explicit.getProducts g

getProductTopOfBook :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => ProductId -> m AggrigateBook
getProductTopOfBook pid = do
    g <- (^. gdax) <$> ask
    Explicit.getProductTopOfBook g pid

getProductTop50OfBook :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => ProductId -> m AggrigateBook
getProductTop50OfBook pid = do
    g <- (^. gdax) <$> ask
    Explicit.getProductTop50OfBook g pid

getProductOrderBook :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => ProductId -> m Book
getProductOrderBook pid = do
    g <- (^. gdax) <$> ask
    Explicit.getProductOrderBook g pid

getCurrencies :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m (Vector Currency)
getCurrencies = do
    g <- (^. gdax) <$> ask
    Explicit.getCurrencies g

getTime :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m UTCTime
getTime = do
    g <- (^. gdax) <$> ask
    Explicit.getTime g
