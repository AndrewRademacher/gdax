{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Network.GDAX.Types.Private where

import           Data.Aeson
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time
import           Data.Typeable
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
