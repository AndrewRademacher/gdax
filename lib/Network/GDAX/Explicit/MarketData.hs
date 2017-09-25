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
import           Data.Aeson.Types
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Data.Vector
import           GHC.Generics
import           Network.GDAX.Core
import           Network.GDAX.Exceptions
import           Text.Read

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show CurrencyId where
    show = show . unCurrencyId

data Currency
    = Currency
        { _currId      :: CurrencyId
        , _currName    :: Text
        , _currMinSize :: Scientific
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Currency where
    parseJSON = withObject "Currency" $ \m -> Currency
        <$> m .: "id"
        <*> m .: "name"
        <*> (m .: "min_size" >>= textScientific)

textScientific :: Value -> Parser Scientific
textScientific = withText "Text Scientific" $ \t ->
    case readMaybe (T.unpack t) of
        Just n  -> pure n
        Nothing -> fail "Could not parse string scientific."

getCurrencies :: (MonadIO m, MonadThrow m) => Gdax -> m (Vector Currency)
getCurrencies g = gdaxGet g "/currencies"

getTime :: (MonadIO m, MonadThrow m) => Gdax -> m UTCTime
getTime g = do
    res <- gdaxGet g "/time"
    case (res :: Value) ^? key "epoch" . _Double of
        Nothing  -> throwM $ MalformedGDAXResponse "Epoch field was either missing or malformed in response from GET /time."
        Just val -> return $ posixSecondsToUTCTime $ realToFrac val
