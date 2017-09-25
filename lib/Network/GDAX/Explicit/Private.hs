{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Explicit.Private where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text
import           Network.GDAX.Core

placeOrder :: (MonadIO m, MonadThrow m) => Gdax -> m Value
placeOrder g = gdaxSignedPost g "/orders" body
    where
        body = object
            [ "size" .= ("0.01" :: Text)
            , "price" .= ("0.100" :: Text)
            , "side" .= ("buy" :: Text)
            , "product_id" .= ("BTC-USD" :: Text)
            ]

listAccounts :: (MonadIO m, MonadThrow m) => Gdax -> m Value
listAccounts g =
    gdaxSignedGet g "/accounts"
