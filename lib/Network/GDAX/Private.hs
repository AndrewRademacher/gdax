{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Private where

import           Control.Lens           hiding ((.=))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text
import           Network.GDAX.Core

-- placeOrder - Example of making authenticated request with body against GDAX.

placeOrder' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m Value
placeOrder' = do
    g <- (^. gdax) <$> ask
    placeOrder g

placeOrder :: (MonadIO m, MonadThrow m) => Gdax -> m Value
placeOrder g = gdaxSignedPost g "/orders" body
    where
        body = object
            [ "size" .= ("0.01" :: Text)
            , "price" .= ("0.100" :: Text)
            , "side" .= ("buy" :: Text)
            , "product_id" .= ("BTC-USD" :: Text)
            ]

-- listAccounts - Example of making an authenticated request against GDAX.

listAccounts' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGdax e) => m Value
listAccounts' = do
    g <- (^. gdax) <$> ask
    listAccounts g

listAccounts :: (MonadIO m, MonadThrow m) => Gdax -> m Value
listAccounts g =
    gdaxSignedGet g "/accounts"
