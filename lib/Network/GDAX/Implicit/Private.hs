{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Implicit.Private where

import           Control.Lens                  hiding ((.=))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Network.GDAX.Core
import qualified Network.GDAX.Explicit.Private as Explicit

placeOrder' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m Value
placeOrder' = do
    g <- (^. gdax) <$> ask
    Explicit.placeOrder g

listAccounts' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m Value
listAccounts' = do
    g <- (^. gdax) <$> ask
    Explicit.listAccounts g