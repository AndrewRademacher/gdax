{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.Private where


import           Data.Aeson
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
