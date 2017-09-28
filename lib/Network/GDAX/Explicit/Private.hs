{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Explicit.Private where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Vector                (Vector)
import           Network.GDAX.Core
import           Network.GDAX.Types.Private
import           Network.GDAX.Types.Shared

listAccounts :: (MonadIO m, MonadThrow m) => Gdax -> m (Vector Account)
listAccounts g = gdaxSignedGet g "/accounts"

getAccount :: (MonadIO m, MonadThrow m) => Gdax -> AccountId -> m Account
getAccount g aid = gdaxSignedGet g ("/accounts/" <> show aid)

getAccountHistory :: (MonadIO m, MonadThrow m) => Gdax -> AccountId -> m (Vector Entry)
getAccountHistory g aid = gdaxSignedGet g ("/accounts/" <> show aid <> "/ledger")

getAccountHolds :: (MonadIO m, MonadThrow m) => Gdax -> AccountId -> m (Vector Hold)
getAccountHolds g aid = gdaxSignedGet g ("/accounts/" <> show aid <> "/holds")

placeOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewOrder -> m NewOrderConfirmation
placeOrder g no = gdaxSignedPost g "/orders" no

placeLimitOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewLimitOrder -> m NewOrderConfirmation
placeLimitOrder g no = gdaxSignedPost g "/orders" no

placeMarketOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewMarketOrder -> m NewOrderConfirmation
placeMarketOrder g no = gdaxSignedPost g "/orders" no

placeStopOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewStopOrder -> m NewOrderConfirmation
placeStopOrder g no = gdaxSignedPost g "/orders" no
