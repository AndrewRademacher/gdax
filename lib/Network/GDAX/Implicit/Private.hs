{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Implicit.Private where

import           Control.Lens                  hiding ((.=))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Set                      (Set)
import           Data.Vector                   (Vector)
import           Network.GDAX.Core
import qualified Network.GDAX.Explicit.Private as Explicit
import           Network.GDAX.Types.Private
import           Network.GDAX.Types.Shared

listAccounts :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m (Vector Account)
listAccounts = do
    g <- (^. gdax) <$> ask
    Explicit.listAccounts g

getAccount :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => AccountId -> m Account
getAccount aid = do
    g <- (^. gdax) <$> ask
    Explicit.getAccount g aid

getAccountHistory :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => AccountId -> m (Vector Entry)
getAccountHistory aid = do
    g <- (^. gdax) <$> ask
    Explicit.getAccountHistory g aid

getAccountHolds :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => AccountId -> m (Vector Hold)
getAccountHolds aid = do
    g <- (^. gdax) <$> ask
    Explicit.getAccountHolds g aid

placeOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => NewOrder -> m NewOrderConfirmation
placeOrder no = do
    g <- (^. gdax) <$> ask
    Explicit.placeOrder g no

placeLimitOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => NewLimitOrder -> m NewOrderConfirmation
placeLimitOrder no = do
    g <- (^. gdax) <$> ask
    Explicit.placeLimitOrder g no

placeMarketOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => NewMarketOrder -> m NewOrderConfirmation
placeMarketOrder no = do
    g <- (^. gdax) <$> ask
    Explicit.placeMarketOrder g no

placeStopOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => NewStopOrder -> m NewOrderConfirmation
placeStopOrder no = do
    g <- (^. gdax) <$> ask
    Explicit.placeStopOrder g no

cancelOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => OrderId -> m ()
cancelOrder oid = do
    g <- (^. gdax) <$> ask
    Explicit.cancelOrder g oid

cancelAllOrders :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => ProductId -> m (Vector OrderId)
cancelAllOrders pid = do
    g <- (^. gdax) <$> ask
    Explicit.cancelAllOrders g pid

listOrders :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => Set ProductId -> Set OrderStatus -> m (Vector Order)
listOrders pids oss = do
    g <- (^. gdax) <$> ask
    Explicit.listOrders g pids oss

getOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => OrderId -> m Order
getOrder oid  = do
    g <- (^. gdax) <$> ask
    Explicit.getOrder g oid

listFills :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => Set OrderId -> Set ProductId -> m (Vector Fill)
listFills oids pids  = do
    g <- (^. gdax) <$> ask
    Explicit.listFills g oids pids

listFundings :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => Set FundingStatus -> m (Vector Funding)
listFundings fs = do
    g <- (^. gdax) <$> ask
    Explicit.listFundings g fs

repayFunding :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => CurrencyId -> Double -> m ()
repayFunding c a = do
    g <- (^. gdax) <$> ask
    Explicit.repayFunding g c a

createMarginTransfer :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => NewMarginTransfer -> m MarginTransfer
createMarginTransfer nmt = do
    g <- (^. gdax) <$> ask
    Explicit.createMarginTransfer g nmt

getPosition :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m Position
getPosition = do
    g <- (^. gdax) <$> ask
    Explicit.getPosition g

closePosition :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => Explicit.RepayOnly -> m ()
closePosition r = do
    g <- (^. gdax) <$> ask
    Explicit.closePosition g r

deposit :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => Deposit -> m DepositReceipt
deposit d = do
    g <- (^. gdax) <$> ask
    Explicit.deposit g d
