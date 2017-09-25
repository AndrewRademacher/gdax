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

-- Product

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show ProductId where
    show = show . unProductId

data Product
    = Product
        { _prodId             :: ProductId
        , _prodBaseCurrency   :: CurrencyId
        , _prodQuoteCurrency  :: CurrencyId
        , _prodBaseMinSize    :: Scientific
        , _prodBaseMaxSize    :: Scientific
        , _prodQuoteIncrement :: Scientific
        , _prodDisplayName    :: Text
        , _prodMarginEnabled  :: Bool
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Product where
    parseJSON = withObject "Product" $ \o -> Product
        <$> o .: "id"
        <*> o .: "base_currency"
        <*> o .: "quote_currency"
        <*> (o .: "base_min_size" >>= textScientific)
        <*> (o .: "base_max_size" >>= textScientific)
        <*> (o .: "quote_increment" >>= textScientific)
        <*> o .: "display_name"
        <*> o .: "margin_enabled"

-- Currency

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
    parseJSON = withObject "Currency" $ \o -> Currency
        <$> o .: "id"
        <*> o .: "name"
        <*> (o .: "min_size" >>= textScientific)
