{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.Private where


import           Data.Aeson
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.GDAX.Parsers
import           Network.GDAX.Types.Shared


data Account
    = Account
        { _accountId        :: AccountId
        , _accountProfileId :: ProfileId
        , _accountCurrency  :: CurrencyId
        , _accountBalance   :: Double
        , _accountAvailable :: Double
        , _accountHold      :: Double
        }
    | MarginAccount
        { _accountId            :: AccountId
        , _accountProfileId     :: ProfileId
        , _accountCurrency      :: CurrencyId
        , _accountBalance       :: Double
        , _accountAvailable     :: Double
        , _accountHold          :: Double
        , _accountFundedAmount  :: Double
        , _accountDefaultAmount :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Account where
    parseJSON = withObject "Account" $ \o -> do
        mar <- o .:? "margin_enabled"
        if mar == (Just True)
            then MarginAccount
                <$> o .: "id"
                <*> o .: "profile_id"
                <*> o .: "currency"
                <*> (o .: "balance" >>= textDouble)
                <*> (o .: "available" >>= textDouble)
                <*> (o .: "hold" >>= textDouble)
                <*> (o .: "funded_amount" >>= textDouble)
                <*> (o .: "default_amount" >>= textDouble)
            else Account
                <$> o .: "id"
                <*> o .: "profile_id"
                <*> o .: "currency"
                <*> (o .: "balance" >>= textDouble)
                <*> (o .: "available" >>= textDouble)
                <*> (o .: "hold" >>= textDouble)

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
