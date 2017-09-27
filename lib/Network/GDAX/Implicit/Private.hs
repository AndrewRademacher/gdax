{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Implicit.Private where

import           Control.Lens                  hiding ((.=))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Vector                   (Vector)
import           Network.GDAX.Core
import qualified Network.GDAX.Explicit.Private as Explicit
import           Network.GDAX.Types.Private
import           Network.GDAX.Types.Shared

placeOrder :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m Value
placeOrder = do
    g <- (^. gdax) <$> ask
    Explicit.placeOrder g

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
