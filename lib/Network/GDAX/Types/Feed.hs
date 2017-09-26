{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.GDAX.Types.Feed where

import           Data.Aeson
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Typeable
import           Data.Vector
import qualified Data.Vector.Generic           as V
import           GHC.Generics
import           Network.GDAX.Parsers
import           Network.GDAX.Types.MarketData

data Subscription
    = Subscription
        { _subProducts :: Vector ProductId
        , _subChannels :: Vector ChannelSubscription
        }
    deriving (Show, Typeable, Generic)


newtype Subscribe = Subscribe { unSubscribe :: Subscription }
    deriving (Show, Typeable, Generic)

instance ToJSON Subscribe where
    toJSON (Subscribe s) = object
        [ "type" .= ("subscribe"::Text)
        , "product_ids" .= _subProducts s
        , "channels" .= _subChannels s
        ]

instance FromJSON Subscribe where
    parseJSON = withObject "Subscribe" $ \o -> do
        t <- o .: "type"
        case t of
            "subscribe" -> do
                prod <- o .: "products"
                chan <- o .: "channels"
                return $ Subscribe $ Subscription prod chan
            _ -> fail $ T.unpack $ "Expected type 'subscribe' got '" <> t <> "'."

newtype UnSubscribe = UnSubscribe { unUnSubscribe :: Subscription }
    deriving (Show, Typeable, Generic)

instance ToJSON UnSubscribe where
    toJSON (UnSubscribe s) = object
        [ "type" .= ("unsubscribe"::Text)
        , "product_ids" .= _subProducts s
        , "channels" .= _subChannels s
        ]

instance FromJSON UnSubscribe where
    parseJSON = withObject "UnSubscribe" $ \o -> do
        t <- o .: "type"
        case t of
            "subscribe" -> do
                prod <- o .: "products"
                chan <- o .: "channels"
                return $ UnSubscribe $ Subscription prod chan
            _ -> fail $ T.unpack $ "Expected type 'subscribe' got '" <> t <> "'."

data Channel
    = Heartbeat
    | Ticker
    | Level2
    | User
    | Matches
    | Full
    deriving (Eq, Ord, Typeable, Generic)

instance Show Channel where
    show Heartbeat = "heartbeat"
    show Ticker    = "ticker"
    show Level2    = "level2"
    show User      = "user"
    show Matches   = "matches"
    show Full      = "full"

instance ToJSON Channel where
    toJSON = String . T.pack . show

instance FromJSON Channel where
    parseJSON = withText "Channel" $ \t ->
        case t of
            "heartbeat" -> pure Heartbeat
            "ticker"    -> pure Ticker
            "level2"    -> pure Level2
            "user"      -> pure User
            "matches"   -> pure Matches
            "full"      -> pure Full
            u -> fail $ T.unpack $ "Received from unsupported channel '" <> u <> "'."

data ChannelSubscription
    = ChannelSubscription
        { _csubChannel  :: Channel
        , _csubProducts :: Vector ProductId
        }
    deriving (Show, Typeable, Generic)

instance ToJSON ChannelSubscription where
    toJSON c | V.null (_csubProducts c) = toJSON (_csubChannel c)
             | otherwise = object
                [ "name" .= _csubChannel c
                , "product_ids" .= _csubProducts c
                ]

instance FromJSON ChannelSubscription where
    parseJSON s@String{} = ChannelSubscription
        <$> parseJSON s
        <*> pure V.empty
    parseJSON (Object o) = ChannelSubscription
        <$> o .: "name"
        <*> o .: "product_ids"
    parseJSON _ = fail "Channel subscription was not a String or Object."

data FeedError
    = FeedError
        { _errMessage  :: Text
        , _errOriginal :: Value
        }
    deriving (Show, Typeable, Generic)

instance FromJSON FeedError where
    parseJSON = withObjectOfType "FeedError" "error" $ \o -> FeedError
        <$> o .: "message"
        <*> o .: "original"
