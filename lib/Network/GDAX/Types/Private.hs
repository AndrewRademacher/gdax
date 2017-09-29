{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Network.GDAX.Types.Private where

import           Data.Aeson
import           Data.HashMap.Strict       (HashMap)
import           Data.Int
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time
import           Data.Typeable
import           Data.Vector               (Vector)
import           GHC.Generics
import           Network.GDAX.Parsers
import           Network.GDAX.Types.Shared
import           Text.Read                 (readMaybe)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text      ()

data Account
    = Account
        { _accountId        :: AccountId
        , _accountProfileId :: ProfileId
        , _accountCurrency  :: CurrencyId
        , _accountBalance   :: Double
        , _accountAvailable :: Double
        , _accountHold      :: Double
        , _accountMargin    :: Maybe MarginAccount
        }
    deriving (Show, Typeable, Generic)

data MarginAccount
    = MarginAccount
        { _maccountFundedAmount  :: Double
        , _maccountDefaultAmount :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Account where
    parseJSON = withObject "Account" $ \o -> do Account
        <$> o .: "id"
        <*> o .: "profile_id"
        <*> o .: "currency"
        <*> (o .: "balance" >>= textDouble)
        <*> (o .: "available" >>= textDouble)
        <*> (o .: "hold" >>= textDouble)
        <*> do enabled <- o .:? "margin_enabled"
               if enabled == (Just True)
                then (\a b -> Just $ MarginAccount a b)
                        <$> (o .: "funded_amount" >>= textDouble)
                        <*> (o .: "default_amount" >>= textDouble)
                else return Nothing

data Entry
    = Entry
        { _entryId        :: EntryId
        , _entryType      :: EntryType
        , _entryCreatedAt :: UTCTime
        , _entryAmount    :: Double
        , _entryBalance   :: Double
        , _entryDetails   :: EntryDetails
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Entry where
    parseJSON = withObject "Entry" $ \o -> Entry
        <$> o .: "id"
        <*> o .: "type"
        <*> o .: "created_at"
        <*> (o .: "amount" >>= textDouble)
        <*> (o .: "balance" >>= textDouble)
        <*> o .: "details"

data EntryDetails
    = EntryDetails
        { _edetailsOrderId   :: Maybe OrderId
        , _edetailsTradeId   :: Maybe TradeId
        , _edetailsProductId :: Maybe ProductId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON EntryDetails where
    parseJSON = withObject "EntryDetails" $ \o -> EntryDetails
        <$> o .:? "order_id"
        <*> o .:? "trade_id"
        <*> o .:? "product_id"

data Hold
    = Hold
        { _holdId        :: HoldId
        , _holdAccountId :: AccountId
        , _holdCreatedAt :: UTCTime
        , _holdUpdatedAt :: UTCTime
        , _holdAmount    :: Double
        , _holdReference :: HoldReference
        }
    deriving (Show, Typeable, Generic)

data HoldReference
    = HoldOrder OrderId
    | HoldTransfer TransferId
    deriving (Show, Typeable, Generic)

instance FromJSON Hold where
    parseJSON = withObject "Hold" $ \o -> Hold
        <$> o .: "id"
        <*> o .: "account_id"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "amount"
        <*> parseRef o
        where
            parseRef o = do
                t <- o .: "type"
                case t of
                    "order" -> HoldOrder <$> o .: "ref"
                    "transfer" -> HoldTransfer <$> o .: "ref"
                    _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid type for hold orders."

data NewOrder
    = NewOrderLimit NewLimitOrder
    | NewOrderMarket NewMarketOrder
    | NewOrderStop NewStopOrder
    deriving (Show, Typeable, Generic)

instance ToJSON NewOrder where
    toJSON (NewOrderLimit o)  = toJSON o
    toJSON (NewOrderMarket o) = toJSON o
    toJSON (NewOrderStop o)   = toJSON o

data NewLimitOrder
    = NewLimitOrder
        { _nloClientOrderId       :: Maybe ClientOrderId
        , _nloSide                :: Side
        , _nloProductId           :: ProductId
        , _nloSelfTradePrevention :: SelfTradePolicy

        , _nloPrice               :: Double
        , _nloSize                :: Double
        , _nloTimeInForce         :: Maybe TimeInForce
        , _nloCancelAfter         :: Maybe CancelAfterPolicy
        , _nloPostOnly            :: Maybe Bool
        }
    deriving (Show, Typeable, Generic)

instance ToJSON NewLimitOrder where
    toJSON NewLimitOrder{..} = object
        [ "client_oid" .= _nloClientOrderId
        , "type" .= ("limit"::Text)
        , "side" .= _nloSide
        , "product_id" .= _nloProductId
        , "stp" .= _nloSelfTradePrevention
        , "price" .= _nloPrice
        , "size" .= _nloSize
        , "time_in_force" .= _nloTimeInForce
        , "cancel_after" .= _nloCancelAfter
        , "post_only" .= _nloPostOnly
        ]

data NewMarketOrder
    = NewMarketOrder
        { _nmoClientOrderId       :: Maybe ClientOrderId
        , _nmoSide                :: Side
        , _nmoProductId           :: ProductId
        , _nmoSelfTradePrevention :: SelfTradePolicy

        , _nmoMarketDesire        :: MarketDesire
        }
    deriving (Show, Typeable, Generic)

instance ToJSON NewMarketOrder where
    toJSON NewMarketOrder{..} = object $
        [ "client_oid" .= _nmoClientOrderId
        , "type" .= ("market"::Text)
        , "side" .= _nmoSide
        , "product_id" .= _nmoProductId
        , "stp" .= _nmoSelfTradePrevention
        ] <> case _nmoMarketDesire of
                (DesireSize s)  -> [ "size" .= s ]
                (DesireFunds f) -> [ "funds" .= f ]

data NewStopOrder
    = NewStopOrder
        { _nsoClientOrderId       :: Maybe ClientOrderId
        , _nsoSide                :: Side
        , _nsoProductId           :: ProductId
        , _nsoSelfTradePrevention :: SelfTradePolicy

        , _nsoPrice               :: Double
        , _nsoMarketDesire        :: MarketDesire
        }
    deriving (Show, Typeable, Generic)

instance ToJSON NewStopOrder where
    toJSON NewStopOrder{..} = object $
        [ "client_oid" .= _nsoClientOrderId
        , "type" .= ("stop"::Text)
        , "side" .= _nsoSide
        , "product_id" .= _nsoProductId
        , "stp" .= _nsoSelfTradePrevention
        , "price" .= _nsoPrice
        ] <> case _nsoMarketDesire of
                (DesireSize s)  -> [ "size" .= s ]
                (DesireFunds f) -> [ "funds" .= f ]

data MarketDesire
    = DesireSize Double -- ^ Desired amount in commodity (e.g. BTC)
    | DesireFunds Double -- ^ Desired amount in quote currency (e.g. USD)
    deriving (Show, Typeable, Generic)

data TimeInForce
    = GoodTillCanceled
    | GoodTillTime
    | ImmediateOrCancel
    | FillOrKill
    deriving (Eq, Ord, Typeable, Generic)

instance Show TimeInForce where
    show GoodTillCanceled  = "GTC"
    show GoodTillTime      = "GTT"
    show ImmediateOrCancel = "IOC"
    show FillOrKill        = "FOK"

instance ToJSON TimeInForce where
    toJSON = String . T.pack . show

instance FromJSON TimeInForce where
    parseJSON = withText "TimeInForce" $ \t ->
        case t of
            "GTC" -> pure GoodTillCanceled
            "GTT" -> pure GoodTillTime
            "IOC" -> pure ImmediateOrCancel
            "FOK" -> pure FillOrKill
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid time in force."

data SelfTradePolicy
    = DecreaseOrCancel
    | CancelOldest
    | CancelNewest
    | CancelBoth
    deriving (Eq, Ord, Typeable, Generic)

instance Show SelfTradePolicy where
    show DecreaseOrCancel = "dc"
    show CancelOldest     = "co"
    show CancelNewest     = "cn"
    show CancelBoth       = "cb"

instance ToJSON SelfTradePolicy where
    toJSON = String . T.pack . show

instance FromJSON SelfTradePolicy where
    parseJSON = withText "SelfTradePolicy" $ \t ->
        case t of
            "dc" -> pure DecreaseOrCancel
            "co" -> pure CancelOldest
            "cn" -> pure CancelNewest
            "cb" -> pure CancelBoth
            _ -> fail $ T.unpack $ "'" <> t <> "' is not a valid self trade policy."

data CancelAfterPolicy
    = CancelAfterMinutes Word
    | CancelAfterHours Word
    | CancelAfterDays Word
    deriving (Typeable, Generic)

instance Show CancelAfterPolicy where
    show (CancelAfterMinutes w) = show w <> " mintes"
    show (CancelAfterHours w)   = show w <> " hours"
    show (CancelAfterDays w)    = show w <> " days"

instance ToJSON CancelAfterPolicy where
    toJSON = String . T.pack . show

instance FromJSON CancelAfterPolicy where
    parseJSON = withText "CancelAfterPolicy" $ \t ->
        case t =~ ("([0-9]+) (days|minutes|hours)"::Text) of
            ((_, _, _, [mv, mtag]) :: (Text, Text, Text, [Text])) ->
                case readMaybe $ T.unpack mv of
                    Just v ->
                        case mtag of
                            "minutes" -> pure $ CancelAfterMinutes v
                            "hours" -> pure $ CancelAfterHours v
                            "days" -> pure $ CancelAfterDays v
                            _ -> fail $ T.unpack $ "'" <> mtag <> "' was not 'minutes', 'hours', or 'days'."
                    Nothing -> fail $ T.unpack $ "'" <> mv <> "' could not be parsed as a word."
            _ -> fail $ T.unpack $ "'" <> t <> "' did not match regex validation."

-- data NewMarginOrder -- Implement with the rest of margin accounts.
--     = NewMarginOrder
--         {
--         }
--     deriving (Show, Typeable, Generic)

data NewOrderConfirmation
    = NewOrderConfirmation
        { _nocOrderId :: OrderId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON NewOrderConfirmation where
    parseJSON = withObject "NewOrderConfirmation" $ \o -> NewOrderConfirmation
        <$> o .: "id"

data Order
    = Order
        { _orderId                  :: OrderId
        , _orderPrice               :: Double
        , _orderSize                :: Double
        , _orderProductId           :: ProductId
        , _orderSide                :: Side
        , _orderSelfTradePrevention :: SelfTradePolicy
        , _orderType                :: OrderType
        , _orderTimeInForce         :: TimeInForce
        , _orderPostOnly            :: Bool
        , _orderCreatedAt           :: UTCTime
        , _orderFillFees            :: Double
        , _orderFilledSize          :: Double
        , _orderExecutedValue       :: Double
        , _orderStatus              :: OrderStatus
        , _orderSettled             :: Bool
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Order where
    parseJSON = withObject "Order" $ \o -> Order
        <$> o .: "id"
        <*> o .: "price"
        <*> o .: "size"
        <*> o .: "product_id"
        <*> o .: "side"
        <*> o .: "stp"
        <*> o .: "type"
        <*> o .: "time_in_force"
        <*> o .: "post_only"
        <*> o .: "created_at"
        <*> o .: "fill_fees"
        <*> o .: "filled_size"
        <*> o .: "executed_value"
        <*> o .: "status"
        <*> o .: "settled"

data Fill
    = Fill
        { _fillTradeId   :: TradeId
        , _fillProductId :: ProductId
        , _fillPrice     :: Double
        , _fillSize      :: Double
        , _fillOrderId   :: OrderId
        , _fillCreatedAt :: UTCTime
        , _fillLiquidity :: Liquidity
        , _fillFee       :: Double
        , _fillSettled   :: Bool
        , _fillSide      :: Side
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Fill where
    parseJSON = withObject "Fill" $ \o -> Fill
        <$> o .: "trade_id"
        <*> o .: "product_id"
        <*> o .: "price"
        <*> o .: "size"
        <*> o .: "order_id"
        <*> o .: "created_at"
        <*> o .: "liquidity"
        <*> o .: "fee"
        <*> o .: "settled"
        <*> o .: "side"

data Funding
    = Funding
        { _fundingId            :: FundingId
        , _fundingOrderId       :: OrderId
        , _fundingProfileId     :: ProfileId
        , _fundingAmount        :: Double
        , _fundingStatus        :: FundingStatus
        , _fundingCreatedAt     :: UTCTime
        , _fundingCurrency      :: CurrencyId
        , _fundingRepaidAmount  :: Double
        , _fundingDefaultAmount :: Double
        , _fundingRepaidDefault :: Bool
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Funding where
    parseJSON = withObject "Funding" $ \o -> Funding
        <$> o .: "id"
        <*> o .: "order_id"
        <*> o .: "profile_id"
        <*> o .: "amount"
        <*> o .: "status"
        <*> o .: "created_at"
        <*> o .: "currency"
        <*> o .: "repaid_amount"
        <*> o .: "default_amount"
        <*> o .: "repaid_default"

data NewMarginTransfer
    = NewMarginTransfer
        { _nmtMarginProfileId :: ProfileId
        , _nmtType            :: MarginType
        , _nmtCurrency        :: CurrencyId
        , _nmtAmount          :: Double
        }
    deriving (Show, Typeable, Generic)

instance ToJSON NewMarginTransfer where
    toJSON NewMarginTransfer{..} = object
        [ "margin_profile_id" .= _nmtMarginProfileId
        , "type" .= _nmtType
        , "currency" .= _nmtCurrency
        , "amount" .= _nmtAmount
        ]

data MarginTransfer
    = MarginTransfer
        { _mtCreatedAt       :: UTCTime
        , _mtId              :: MarginTransferId
        , _mtUserId          :: UserId
        , _mtProfileId       :: ProfileId
        , _mtMarginProfileId :: ProfileId
        , _mtType            :: MarginType
        , _mtAmount          :: Double
        , _mtCurrency        :: CurrencyId
        , _mtAccountId       :: AccountId
        , _mtMarginAccountId :: AccountId
        , _mtMarginProductId :: ProductId
        , _mtStatus          :: MarginStatus
        , _mtNonce           :: Int64
        }
    deriving (Show, Typeable, Generic)

instance FromJSON MarginTransfer where
    parseJSON = withObject "MarginTransfer" $ \o -> MarginTransfer
        <$> o .: "created_at"
        <*> o .: "id"
        <*> o .: "user_id"
        <*> o .: "profile_id"
        <*> o .: "margin_profile_id"
        <*> o .: "type"
        <*> o .: "amount"
        <*> o .: "currency"
        <*> o .: "accountId"
        <*> o .: "margin_account_id"
        <*> o .: "margin_product_id"
        <*> o .: "status"
        <*> o .: "nonce"

data Position
    = Position
        { _posStatus       :: PositionStatus
        , _posFunding      :: PositionFunding
        , _posAccounts     :: HashMap Text PositionAccount -- ^ Text should be CurrencyId, but it will have to be fixed later.
        , _posMarginCall   :: MarginCall
        , _posUserId       :: UserId
        , _posProfileId    :: ProfileId
        , _posPositionInfo :: PositionInfo
        , _posProductId    :: ProductId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Position where
    parseJSON = withObject "Position" $ \o -> Position
        <$> o .: "status"
        <*> o .: "funding"
        <*> o .: "accounts"
        <*> o .: "margin_call"
        <*> o .: "user_id"
        <*> o .: "profile_id"
        <*> o .: "position"
        <*> o .: "product_id"

data PositionFunding
    = PositionFunding
        { _posfunMaxFundingValue   :: Double
        , _posfunFundingValue      :: Double
        , _posfunOldestOutstanding :: OutstandingFunding
        }
    deriving (Show, Typeable, Generic)

instance FromJSON PositionFunding where
    parseJSON = withObject "PositionFunding" $ \o -> PositionFunding
        <$> o .: "max_funding_value"
        <*> o .: "funding_value"
        <*> o .: "oldest_outstanding"

data OutstandingFunding
    = OutstandingFunding
        { _ofunId        :: FundingId
        , _ofunOrderId   :: OrderId
        , _ofunCreatedAt :: UTCTime
        , _ofunCurrency  :: CurrencyId
        , _ofunAccountId :: AccountId
        , _ofunAmount    :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON OutstandingFunding where
    parseJSON = withObject "OutstandingFunding" $ \o -> OutstandingFunding
        <$> o .: "id"
        <*> o .: "order_id"
        <*> o .: "created_at"
        <*> o .: "currency"
        <*> o .: "account_id"
        <*> o .: "amount"

data PositionAccount
    = PositionAccount
        { _paccountId            :: AccountId
        , _paccountBalance       :: Double
        , _paccountHold          :: Double
        , _paccountFundedAmount  :: Double
        , _paccountDefaultAmount :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON PositionAccount where
    parseJSON = withObject "PositionAccount" $ \o -> PositionAccount
        <$> o .: "id"
        <*> o .: "balance"
        <*> o .: "hold"
        <*> o .: "funded_amount"
        <*> o .: "default_amount"

data MarginCall
    = MarginCall
        { _mcallActive :: Bool
        , _mcallPrice  :: Double
        , _mcallSide   :: Side
        , _mcallSize   :: Double
        , _mcallFunds  :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON MarginCall where
    parseJSON = withObject "MarginCall" $ \o -> MarginCall
        <$> o .: "active"
        <*> o .: "price"
        <*> o .: "side"
        <*> o .: "size"
        <*> o .: "funds"

data PositionInfo
    = PositionInfo
        { _pinfoType       :: PositionType
        , _pinfoSize       :: Double
        , _pinfoComplement :: Double
        , _pinfoMaxSize    :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON PositionInfo where
    parseJSON = withObject "PositionInfo" $ \o -> PositionInfo
        <$> o .: "type"
        <*> o .: "size"
        <*> o .: "complement"
        <*> o .: "max_size"

data Deposit
    = Deposit
        { _depositAmount        :: Double
        , _depositCurrency      :: CurrencyId
        , _depositPaymentMethod :: PaymentMethodId
        }
    deriving (Show, Typeable, Generic)

instance ToJSON Deposit where
    toJSON Deposit{..} = object
        [ "amount" .= _depositAmount
        , "currency" .= _depositCurrency
        , "payment_method_id" .= _depositPaymentMethod
        ]

data DepositReceipt
    = DepositReceipt
        { _dreceiptId       :: DepositId
        , _dreceiptAmount   :: Double
        , _dreceiptCurrency :: CurrencyId
        , _dreceiptPayoutAt :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance FromJSON DepositReceipt where
    parseJSON = withObject "DepositReceipt" $ \o -> DepositReceipt
        <$> o .: "id"
        <*> o .: "amount"
        <*> o .: "currency"
        <*> o .: "payput_at"

data CoinbaseDeposit
    = CoinbaseDeposit
        { _cdepositAmount          :: Double
        , _cdepositCurrency        :: CurrencyId
        , _cdepositCoinbaseAccount :: AccountId
        }
    deriving (Show, Typeable, Generic)

instance ToJSON CoinbaseDeposit where
    toJSON CoinbaseDeposit{..} = object
        [ "amount" .= _cdepositAmount
        , "currency" .= _cdepositCurrency
        , "coinbase_account_id" .= _cdepositCoinbaseAccount
        ]

data CoinbaseDepositReceipt
    = CoinbaseDepositReceipt
        { _cdreceiptId       :: DepositId
        , _cdreceiptAmount   :: Double
        , _cdreceiptCurrency :: CurrencyId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON CoinbaseDepositReceipt where
    parseJSON = withObject "CoinbaseDepositReceipt" $ \o -> CoinbaseDepositReceipt
        <$> o .: "id"
        <*> o .: "amount"
        <*> o .: "currency"

data Withdraw
    = Withdraw
        { _withdrawAmount        :: Double
        , _withdrawCurrency      :: CurrencyId
        , _withdrawPaymentMethod :: PaymentMethodId
        }
    deriving (Show, Typeable, Generic)

instance ToJSON Withdraw where
    toJSON Withdraw{..} = object
        [ "amount" .= _withdrawAmount
        , "currency" .= _withdrawCurrency
        , "payment_method_id" .= _withdrawPaymentMethod
        ]

data WithdrawReceipt
    = WithdrawReceipt
        { _wreceiptId       :: WithdrawId
        , _wreceiptAmount   :: Double
        , _wreceiptCurrency :: CurrencyId
        , _wreceiptPayoutAt :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance FromJSON WithdrawReceipt where
    parseJSON = withObject "WithdrawReceipt" $ \o -> WithdrawReceipt
        <$> o .: "id"
        <*> o .: "amount"
        <*> o .: "currency"
        <*> o .: "payout_at"

data CoinbaseWithdraw
    = CoinbaseWithdraw
        { _cwithdrawAmount          :: Double
        , _cwithdrawCurrency        :: CurrencyId
        , _cwithdrawCoinbaseAccount :: AccountId
        }
    deriving (Show, Typeable, Generic)

instance ToJSON CoinbaseWithdraw where
    toJSON CoinbaseWithdraw{..} = object
        [ "amount" .= _cwithdrawAmount
        , "currency" .= _cwithdrawCurrency
        , "payment_method_id" .= _cwithdrawCoinbaseAccount
        ]

data CoinbaseWithdrawReceipt
    = CoinbaseWithdrawReceipt
        { _cwreceiptId       :: WithdrawId
        , _cwreceiptAmount   :: Double
        , _cwreceiptCurrency :: CurrencyId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON CoinbaseWithdrawReceipt where
    parseJSON = withObject "CoinbaseWithdrawReceipt" $ \o -> CoinbaseWithdrawReceipt
        <$> o .: "id"
        <*> o .: "amount"
        <*> o .: "currency"

data CryptoWithdraw
    = CryptoWithdraw
        { _crwithdrawAmount          :: Double
        , _crwithdrawCurrency        :: CurrencyId
        , _crwithdrawCoinbaseAccount :: AccountId
        }
    deriving (Show, Typeable, Generic)

instance ToJSON CryptoWithdraw where
    toJSON CryptoWithdraw{..} = object
        [ "amount" .= _crwithdrawAmount
        , "currency" .= _crwithdrawCurrency
        , "payment_method_id" .= _crwithdrawCoinbaseAccount
        ]

data CryptoWithdrawReceipt
    = CryptoWithdrawReceipt
        { _crwreceiptId       :: WithdrawId
        , _crwreceiptAmount   :: Double
        , _crwreceiptCurrency :: CurrencyId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON CryptoWithdrawReceipt where
    parseJSON = withObject "CryptoWithdrawReceipt" $ \o -> CryptoWithdrawReceipt
        <$> o .: "id"
        <*> o .: "amount"
        <*> o .: "currency"

data PaymentMethod
    = PaymentMethod
        { _pmethId            :: PaymentMethodId
        , _pmethType          :: PaymentMethodType
        , _pmethName          :: Text
        , _pmethCurrency      :: CurrencyId
        , _pmethPrimaryBuy    :: Bool
        , _pmethPrimarySell   :: Bool
        , _pmethAllowBuy      :: Bool
        , _pmethAllowSell     :: Bool
        , _pmethAllowDeposit  :: Bool
        , _pmethAllowWithdraw :: Bool
        , _pmethLimits        :: Limits
        }
    deriving (Show, Typeable, Generic)

instance FromJSON PaymentMethod where
    parseJSON = withObject "PaymentMethod" $ \o -> PaymentMethod
        <$> o .: "id"
        <*> o .: "type"
        <*> o .: "name"
        <*> o .: "currency"
        <*> o .: "primary_buy"
        <*> o .: "primary_sell"
        <*> o .: "allow_buy"
        <*> o .: "allow_sell"
        <*> o .: "allow_deposit"
        <*> o .: "allow_withdraw"
        <*> o .: "limits"

data Limits
    = Limits
        { _limitsBuy        :: Vector Limit
        , _limitsInstantBuy :: Vector Limit
        , _limitsSell       :: Vector Limit
        , _limitsDeposit    :: Vector Limit
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Limits where
    parseJSON = withObject "Limits" $ \o -> Limits
        <$> (nothingToEmptyVector <$> o .:? "buy")
        <*> (nothingToEmptyVector <$> o .:? "instant_buy")
        <*> (nothingToEmptyVector <$> o .:? "sell")
        <*> (nothingToEmptyVector <$> o .:? "deposit")

data Limit
    = Limit
        { _limitPeriodInDays :: Word
        , _limitTotal        :: LimitValue
        , _limitRemaining    :: LimitValue
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Limit where
    parseJSON = withObject "Limit" $ \o -> Limit
        <$> o .: "period_in_days"
        <*> o .: "total"
        <*> o .: "remaining"

data LimitValue
    = LimitValue
        { _lvalAmount   :: Double
        , _lvalCurrency :: CurrencyId
        }
    deriving (Show, Typeable, Generic)

instance FromJSON LimitValue where
    parseJSON = withObject "LimitValue" $ \o -> LimitValue
        <$> (o .: "amount" >>= textDouble)
        <*> o .: "currency"

data CoinbaseAccount
    = CoinbaseAccount
        { _cbaccountId              :: AccountId
        , _cbaccountName            :: Text
        , _cbaccountBalance         :: Double
        , _cbaccountCurrency        :: CurrencyId
        , _cbaccountType            :: CoinbaseAccountType
        , _cbaccountPrimary         :: Bool
        , _cbaccountActive          :: Bool
        , _cbaccountWireDepositInfo :: Maybe WireDepositInfo
        , _cbaccountSepaDepositInfo :: Maybe SepaDepositInfo
        }
    deriving (Show, Typeable, Generic)

instance FromJSON CoinbaseAccount where
    parseJSON = withObject "CoinbaseAccount" $ \o -> CoinbaseAccount
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "balance"
        <*> o .: "currency"
        <*> o .: "type"
        <*> o .: "primary"
        <*> o .: "active"
        <*> o .:? "wire_deposit_information"
        <*> o .:? "sepa_deposit_information"

data WireDepositInfo
    = WireDepositInfo
        { _wdiAccountNumber  :: Integer
        , _wdiRoutingNumber  :: Integer
        , _wdiBankName       :: Text
        , _wdiBankAddress    :: Text
        , _wdiBankCountry    :: Country
        , _wdiAccountName    :: Text
        , _wdiAccountAddress :: Text
        , _wdiReference      :: Text
        }
    deriving (Show, Typeable, Generic)

instance FromJSON WireDepositInfo where
    parseJSON = withObject "WireDepositInfo" $ \o -> WireDepositInfo
        <$> o .: "account_number"
        <*> o .: "routing_number"
        <*> o .: "bank_name"
        <*> o .: "bank_address"
        <*> o .: "bank_country"
        <*> o .: "account_name"
        <*> o .: "account_address"
        <*> o .: "reference"

data Country
    = Country
        { _countryCode :: Text
        , _countryName :: Text
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Country where
    parseJSON = withObject "Country" $ \o -> Country
        <$> o .: "code"
        <*> o .: "name"

data SepaDepositInfo
    = SepaDepositInfo
        { _sepaIban            :: Text
        , _sepaSwift           :: Text
        , _sepaBankName        :: Text
        , _sepaBankAddress     :: Text
        , _sepaBankCountryName :: Text
        , _sepaAccountName     :: Text
        , _sepaAccountAddress  :: Text
        , _sepaReference       :: Text
        }
    deriving (Show, Typeable, Generic)

instance FromJSON SepaDepositInfo where
    parseJSON = withObject "SepaDepositInfo" $ \o -> SepaDepositInfo
        <$> o .: "iban"
        <*> o .: "swift"
        <*> o .: "bank_name"
        <*> o .: "bank_address"
        <*> o .: "bank_country_name"
        <*> o .: "account_name"
        <*> o .: "account_address"
        <*> o .: "reference"

data NewReport
    = NewReport
        { _nreportType      :: ReportType
        , _nreportStartDate :: UTCTime
        , _nreportEndDate   :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance ToJSON NewReport where
    toJSON NewReport{..} = object
        [ "type" .= _nreportType
        , "start_date" .= _nreportStartDate
        , "end_date" .= _nreportEndDate
        ]

data Report
    = Report
        { _reportId          :: ReportId
        , _reportType        :: ReportType
        , _reportStatus      :: ReportStatus
        , _reportCreatedAt   :: UTCTime
        , _reportCompletedAt :: Maybe UTCTime
        , _reportExpiresAt   :: UTCTime
        , _reportFileUrl     :: Maybe Text
        , _reportParams      :: ReportParams
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Report where
    parseJSON = withObject "Report" $ \o -> Report
        <$> o .: "id"
        <*> o .: "type"
        <*> o .: "status"
        <*> o .: "created_at"
        <*> o .: "completed_at"
        <*> o .: "expires_at"
        <*> o .: "file_url"
        <*> o .: "params"

data ReportParams
    = ReportParams
        { _rparamStartDate :: UTCTime
        , _rparamEndDate   :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance FromJSON ReportParams where
    parseJSON = withObject "ReportParmas" $ \o -> ReportParams
        <$> o .: "start_date"
        <*> o .: "end_date"

data TrailingVolume
    = TrailingVolume
        { _tvProductId      :: ProductId
        , _tvExchangeVolume :: Double
        , _tvVolume         :: Double
        , _tvRecorededAt    :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance FromJSON TrailingVolume where
    parseJSON = withObject "TrailingVolume" $ \o -> TrailingVolume
        <$> o .: "product_id"
        <*> o .: "exchange_volume"
        <*> o .: "volume"
        <*> o .: "recorded_at"
