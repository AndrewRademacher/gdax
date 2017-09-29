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
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON, IsString)

instance Show UserId where
    show = T.unpack . unUserId

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
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON, IsString)

instance Show StopType where
    show = T.unpack . unStopType

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON, IsString)

instance Show ProductId where
    show = T.unpack . unProductId

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
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON, IsString)

instance Show CurrencyId where
    show = T.unpack . unCurrencyId

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

newtype FundingId = FundingId { unFundingId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show FundingId where
    show = show . unFundingId

data FundingStatus
    = FundingOutstanding
    | FundingSettled
    | FundingRejected
    deriving (Typeable, Generic)

instance Show FundingStatus where
    show FundingOutstanding = "outstanding"
    show FundingSettled     = "settled"
    show FundingRejected    = "rejected"

instance ToJSON FundingStatus where
    toJSON = String . T.pack . show

instance FromJSON FundingStatus where
    parseJSON = withText "FundingStatus" $ \t ->
        case t of
            "outstanding" -> pure FundingOutstanding
            "settled" -> pure FundingSettled
            "rejected" -> pure FundingRejected
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid funding status."

data MarginType
    = MarginDeposit
    | MarginWithdraw
    deriving (Typeable, Generic)

instance Show MarginType where
    show MarginDeposit  = "deposit"
    show MarginWithdraw = "withdraw"

instance ToJSON MarginType where
    toJSON = String . T.pack . show

instance FromJSON MarginType where
    parseJSON = withText "MarginType" $ \t ->
        case t of
            "deposit"  -> pure MarginDeposit
            "withdraw" -> pure MarginWithdraw
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid margin type."

newtype MarginTransferId = MarginTransferId { unMarginTransferId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show MarginTransferId where
    show = show . unMarginTransferId

data MarginStatus
    = MarginCompleted
    deriving (Typeable, Generic)

instance Show MarginStatus where
    show MarginCompleted  = "completed"

instance ToJSON MarginStatus where
    toJSON = String . T.pack . show

instance FromJSON MarginStatus where
    parseJSON = withText "MarginStatus" $ \t ->
        case t of
            "completed"  -> pure MarginCompleted
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid margin status."

data PositionStatus
    = PositionActive
    | PositionPending
    | PositionLocked
    | PositionDefault
    deriving (Typeable, Generic)

instance Show PositionStatus where
    show PositionActive  = "active"
    show PositionPending = "pending"
    show PositionLocked  = "locked"
    show PositionDefault = "default"

instance ToJSON PositionStatus where
    toJSON = String . T.pack . show

instance FromJSON PositionStatus where
    parseJSON = withText "PositionStatus" $ \t ->
        case t of
            "active" -> pure PositionActive
            "pending" -> pure PositionPending
            "locked" -> pure PositionLocked
            "default" -> pure PositionDefault
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid margin status."

data PositionType
    = PositionLong
    | PositionShort
    deriving (Typeable, Generic)

instance Show PositionType where
    show PositionLong  = "long"
    show PositionShort = "short"

instance ToJSON PositionType where
    toJSON = String . T.pack . show

instance FromJSON PositionType where
    parseJSON = withText "PositionType" $ \t ->
        case t of
            "long" -> pure PositionLong
            "short" -> pure PositionShort
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid margin status."

newtype PaymentMethodId = PaymentMethodId { unPaymentMethodId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show PaymentMethodId where
    show = show . unPaymentMethodId

newtype DepositId = DepositId { unDepositId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show DepositId where
    show = show . unDepositId

newtype WithdrawId = WithdrawId { unWithdrawId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show WithdrawId where
    show = show . unWithdrawId

data PaymentMethodType
    = MethodFiatAccount
    | MethodBankWire
    | MethodACHBankAccount
    deriving (Eq, Ord, Typeable, Generic)

instance Show PaymentMethodType where
    show MethodFiatAccount    = "fiat_account"
    show MethodBankWire       = "bank_wire"
    show MethodACHBankAccount = "ach_bank_account"

instance ToJSON PaymentMethodType where
    toJSON = String . T.pack . show

instance FromJSON PaymentMethodType where
    parseJSON = withText "PaymentMethodType" $ \t ->
        case t of
            "fiat_account" -> pure MethodFiatAccount
            "bank_wire" -> pure MethodBankWire
            "ach_bank_account" -> pure MethodACHBankAccount
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid payment method type."

data CoinbaseAccountType
    = CBTypeWallet
    | CBTypeFiat
    deriving (Typeable, Generic)

instance Show CoinbaseAccountType where
    show CBTypeWallet = "wallet"
    show CBTypeFiat   = "fiat"

instance ToJSON CoinbaseAccountType where
    toJSON = String . T.pack . show

instance FromJSON CoinbaseAccountType where
    parseJSON = withText "CoinbaseAccountType" $ \t ->
        case t of
            "wallet" -> pure CBTypeWallet
            "fiat" -> pure CBTypeFiat
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid coinbase account type."

newtype ReportId = ReportId { unReportId :: UUID }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show ReportId where
    show = show . unReportId

data ReportType
    = ReportFills
    | ReportAccount
    deriving (Typeable, Generic)

instance Show ReportType where
    show ReportFills   = "fills"
    show ReportAccount = "account"

instance ToJSON ReportType where
    toJSON = String . T.pack . show

instance FromJSON ReportType where
    parseJSON = withText "ReportType" $ \t ->
        case t of
            "fills" -> pure ReportFills
            "account" -> pure ReportAccount
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid report type."

data ReportStatus
    = ReportPending
    | ReportCreating
    | ReportReady
    deriving (Typeable, Generic)

instance Show ReportStatus where
    show ReportPending  = "pending"
    show ReportCreating = "creating"
    show ReportReady    = "ready"

instance ToJSON ReportStatus where
    toJSON = String . T.pack . show

instance FromJSON ReportStatus where
    parseJSON = withText "ReportStatus" $ \t ->
        case t of
            "pending" -> pure ReportPending
            "creating" -> pure ReportCreating
            "ready" -> pure ReportReady
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid report status."

