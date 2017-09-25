{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.MarketData where

import           Data.Aeson
import           Data.Scientific
import           Data.Text            (Text)
import           Data.Typeable
import           GHC.Generics
import           Network.GDAX.Parsers

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
