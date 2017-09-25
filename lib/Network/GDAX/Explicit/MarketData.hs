{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Explicit.MarketData where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Vector
import           Network.GDAX.Core
import           Network.GDAX.Exceptions
import           Network.GDAX.Types.MarketData
import           Network.Wreq

getProducts :: (MonadIO m, MonadThrow m) => Gdax -> m (Vector Product)
getProducts g = gdaxGet g "/products"

getProductTopOfBook :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> m AggrigateBook
getProductTopOfBook g pid = gdaxGet g ("/products/" <> T.unpack (unProductId pid) <> "/book")

getProductTop50OfBook :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> m AggrigateBook
getProductTop50OfBook g pid = gdaxGet g ("/products/" <> T.unpack (unProductId pid) <> "/book?level=2")

getProductOrderBook :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> m Book
getProductOrderBook g pid = gdaxGet g ("/products/" <> T.unpack (unProductId pid) <> "/book?level=3")

getProductTicker :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> m Tick
getProductTicker g pid = gdaxGet g ("/products/" <> T.unpack (unProductId pid) <> "/ticker")

getProductTrades :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> m (Vector Trade)
getProductTrades g pid = gdaxGet g ("/products/" <> T.unpack (unProductId pid) <> "/trades")

getProductHistory :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> Maybe StartTime -> Maybe EndTime -> Maybe Granularity -> m (Vector Candle)
getProductHistory g pid mst met mg = gdaxGetWith g ("/products" <> T.unpack (unProductId pid) <> "/candles") opts
    where
        opts = defaults
                & param "start" .~ nothingToEmpty (fmt <$> mst)
                & param "end" .~ nothingToEmpty (fmt <$> met)
                & param "granularity" .~ nothingToEmpty (T.pack . show <$> mg)
        nothingToEmpty (Just v) = [ v ]
        nothingToEmpty Nothing  = []
        fmt t  = let t' = formatTime defaultTimeLocale "%FT%T." t
                 in T.pack $ t' <> Prelude.take 6 (formatTime defaultTimeLocale "%q" t) <> "Z"

getCurrencies :: (MonadIO m, MonadThrow m) => Gdax -> m (Vector Currency)
getCurrencies g = gdaxGet g "/currencies"

getTime :: (MonadIO m, MonadThrow m) => Gdax -> m UTCTime
getTime g = do
    res <- gdaxGet g "/time"
    case (res :: Value) ^? key "epoch" . _Double of
        Nothing  -> throwM $ MalformedGDAXResponse "Epoch field was either missing or malformed in response from GET /time."
        Just val -> return $ posixSecondsToUTCTime $ realToFrac val
