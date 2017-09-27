{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.Shared where

import           Data.Aeson
import           Data.Int
import           Data.Monoid
import           Data.String
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Typeable
import           Data.UUID
import           GHC.Generics

newtype AccountId = AccountId { unAccountId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show AccountId where
    show = show . unAccountId

newtype UserId = UserId { unUserId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show UserId where
    show = show . unUserId

newtype ProfileId = ProfileId { unProfileId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show ProfileId where
    show = show . unProfileId


newtype OrderId = OrderId { unOrderId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show OrderId where
    show = show . unOrderId


data OrderType
    = OrderLimit
    | OrderMarket
    deriving (Typeable, Generic)

instance Show OrderType where
    show OrderLimit  = "limit"
    show OrderMarket = "market"

instance FromJSON OrderType where
    parseJSON = withText "OrderType" $ \t ->
        case t of
            "limit"  -> pure OrderLimit
            "market" -> pure OrderMarket
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid order type."

newtype StopType = StopType { unStopType :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show StopType where
    show = show . unStopType

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON, IsString)

instance Show ProductId where
    show = show . unProductId

newtype Sequence = Sequence { unSequence :: Int64 }
    deriving (Eq, Ord, Enum, Typeable, Generic, ToJSON, FromJSON)

instance Show Sequence where
    show = show . unSequence



newtype TradeId = TradeId { unTradeId :: Int64 }
    deriving (Eq, Ord, Enum, Typeable, Generic, ToJSON, FromJSON)

instance Show TradeId where
    show = show . unTradeId

data Side
    = Buy
    | Sell
    deriving (Show, Typeable, Generic)

instance FromJSON Side where
    parseJSON = withText "Side" $ \t ->
        case t of
            "buy"  -> pure Buy
            "sell" -> pure Sell
            _      -> fail "Side was not either buy or sell."

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show CurrencyId where
    show = show . unCurrencyId

