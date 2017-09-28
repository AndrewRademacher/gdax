{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.Shared where

import           Data.Aeson
import           Data.Int
import           Data.Monoid
import           Data.Scientific
import           Data.String
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Typeable
import           Data.UUID
import           GHC.Generics
import           Text.Read       (readMaybe)

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
    deriving (Eq, Ord, Enum, Typeable, Generic)

instance Show TradeId where
    show = show . unTradeId

instance FromJSON TradeId where
    parseJSON (String s) = case readMaybe $ T.unpack s of
                            Nothing -> fail "TradeId string could not be read as integer."
                            Just v -> pure $ TradeId v
    parseJSON (Number n) = case toBoundedInteger n of
                            Nothing -> fail "TradeId scientific could not be converted into an integer."
                            Just v -> pure $ TradeId v
    parseJSON _ = fail "TradeId can only accept a number or string."

data Side
    = Buy
    | Sell
    deriving (Typeable, Generic)

instance Show Side where
    show Buy  = "buy"
    show Sell = "sell"

instance ToJSON Side where
    toJSON = String . T.pack . show

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

newtype EntryId = EntryId { unEntryId :: Int64 }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show EntryId where
    show = show . unEntryId

data EntryType
    = EntryMatch
    | EntryFee
    | EntryTransfer
    deriving (Eq, Typeable, Generic)

instance Show EntryType where
    show EntryMatch    = "match"
    show EntryFee      = "fee"
    show EntryTransfer = "transfer"

instance FromJSON EntryType where
    parseJSON = withText "EntryType" $ \t ->
        case t of
            "match"    -> pure EntryMatch
            "fee"      -> pure EntryFee
            "transfer" -> pure EntryTransfer
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid entry type."

newtype TransferId = TransferId { unTransferId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show TransferId where
    show = show . unTransferId

newtype HoldId = HoldId { unHoldId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show HoldId where
    show = show . unHoldId

newtype ClientOrderId = ClientOrderId { unClientOrderId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show ClientOrderId where
    show = show . unClientOrderId

data OrderStatus
    = OrderOpen
    | OrderPending
    | OrderActive
    | OrderDone
    | OrderSettled
    deriving (Typeable, Generic)

instance Show OrderStatus where
    show OrderOpen    = "open"
    show OrderPending = "pending"
    show OrderActive  = "active"
    show OrderDone    = "done"
    show OrderSettled = "settled"

instance ToJSON OrderStatus where
    toJSON = String . T.pack . show

instance FromJSON OrderStatus where
    parseJSON = withText "OrderStatus" $ \s ->
        case s of
            "open"    -> pure OrderOpen
            "pending" -> pure OrderPending
            "active"  -> pure OrderActive
            "done"    -> pure OrderDone
            "settled" -> pure OrderSettled
            _ -> fail $ T.unpack $ "'" <> s <> "' is not a valid order status."

data Liquidity
    = LiquidityMaker
    | LiquidityTaker
    deriving (Typeable, Generic)

instance Show Liquidity where
    show LiquidityMaker = "M"
    show LiquidityTaker = "T"

instance ToJSON Liquidity where
    toJSON = String . T.pack . show

instance FromJSON Liquidity where
    parseJSON = withText "Liquidity" $ \t ->
        case t of
            "M" -> pure LiquidityMaker
            "T" -> pure LiquidityTaker
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid liquidity."
