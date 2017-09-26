{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.GDAX.Types.Feed where

import           Data.Aeson
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
import           Data.Typeable
import           Data.UUID
import           Data.Vector
import qualified Data.Vector.Generic           as V
import           GHC.Generics
import           Network.GDAX.Parsers
import           Network.GDAX.Types.MarketData

data Subscriptions
    = Subscriptions
        { _subProducts :: Vector ProductId
        , _subChannels :: Vector ChannelSubscription
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Subscriptions where
    parseJSON = withObjectOfType "Subscriptions" "subscriptions" $ \o -> Subscriptions
        <$> (nothingToEmptyVector <$> o .:? "products")
        <*> o .: "channels"

newtype Subscribe = Subscribe { unSubscribe :: Subscriptions }
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
                prod <- nothingToEmptyVector <$>  o .:? "products"
                chan <- o .: "channels"
                return $ Subscribe $ Subscriptions prod chan
            _ -> fail $ T.unpack $ "Expected type 'subscribe' got '" <> t <> "'."

newtype UnSubscribe = UnSubscribe { unUnSubscribe :: Subscriptions }
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
                prod <- nothingToEmptyVector <$> o .:? "products"
                chan <- o .: "channels"
                return $ UnSubscribe $ Subscriptions prod chan
            _ -> fail $ T.unpack $ "Expected type 'subscribe' got '" <> t <> "'."

data Channel
    = ChannelHeartbeat
    | ChannelTicker
    | ChannelLevel2
    | ChannelUser
    | ChannelMatches
    | ChannelFull
    deriving (Eq, Ord, Typeable, Generic)

instance Show Channel where
    show ChannelHeartbeat = "heartbeat"
    show ChannelTicker    = "ticker"
    show ChannelLevel2    = "level2"
    show ChannelUser      = "user"
    show ChannelMatches   = "matches"
    show ChannelFull      = "full"

instance ToJSON Channel where
    toJSON = String . T.pack . show

instance FromJSON Channel where
    parseJSON = withText "Channel" $ \t ->
        case t of
            "heartbeat" -> pure ChannelHeartbeat
            "ticker"    -> pure ChannelTicker
            "level2"    -> pure ChannelLevel2
            "user"      -> pure ChannelUser
            "matches"   -> pure ChannelMatches
            "full"      -> pure ChannelFull
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

data Heartbeat
    = Heartbeat
        { _beatSequence    :: Sequence
        , _beatLastTradeId :: TradeId
        , _beatProductId   :: ProductId
        , _beatTime        :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Heartbeat where
    parseJSON = withObjectOfType "Heartbeat" "heartbeat" $ \o -> Heartbeat
        <$> o .: "sequence"
        <*> o .: "last_trade_id"
        <*> o .: "product_id"
        <*> o .: "time"

data Ticker
    = Ticker
        { _tickerSequence      :: Sequence
        , _tickerProductId     :: ProductId
        , _tickerPrice         :: Double
        , _tickerOpen24Hours   :: Double
        , _tickerVolume24Hours :: Double
        , _tickerLow24Hours    :: Double
        , _tickerHigh24Hours   :: Double
        , _tickerVolume30Days  :: Double
        , _tickerBestBid       :: Double
        , _tickerBestAsk       :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Ticker where
    parseJSON = withObjectOfType "Ticker" "ticker" $ \o -> Ticker
        <$> o .: "sequence"
        <*> o .: "product_id"
        <*> (o .: "price" >>= textDouble)
        <*> (o .: "open_24h" >>= textDouble)
        <*> (o .: "volume_24h" >>= textDouble)
        <*> (o .: "low_24h" >>= textDouble)
        <*> (o .: "high_24h" >>= textDouble)
        <*> (o .: "volume_30d" >>= textDouble)
        <*> (o .: "best_bid" >>= textDouble)
        <*> (o .: "best_ask" >>= textDouble)

data Level2Snapshot
    = Level2Snapshot
        { _l2snapProductId :: ProductId
        , _l2snapBids      :: Vector Level2Item
        , _l2snapAsks      :: Vector Level2Item
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Level2Snapshot where
    parseJSON = withObjectOfType "Level2Snapshot" "snapshot" $ \o -> Level2Snapshot
        <$> o .: "product_id"
        <*> o .: "bids"
        <*> o .: "asks"

data Level2Item
    = Level2Item
        { _l2itemPrice :: {-# UNPACK #-} Double
        , _l2itemSize  :: {-# UNPACK #-} Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Level2Item where
    parseJSON = withArray "Level2Item" $ \a -> Level2Item
        <$> (unStringDouble <$> parseJSON (a V.! 0))
        <*> (unStringDouble <$> parseJSON (a V.! 1))

data Level2Update
    = Level2Update
        { _l2updateProductId :: ProductId
        , _l2updateChanges   :: Vector Level2Change
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Level2Update where
    parseJSON = withObjectOfType "Level2Update" "l2update" $ \o -> Level2Update
        <$> o .: "product_id"
        <*> o .: "changes"

data Level2Change
    = Level2Change
        { _l2bidSide  :: Side
        , _l2bidPrice :: {-# UNPACK #-} Double
        , _l2bidSize  :: {-# UNPACK #-} Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Level2Change where
    parseJSON = withArray "Level2Bid" $ \a -> Level2Change
        <$> parseJSON (a V.! 0)
        <*> (unStringDouble <$> parseJSON (a V.! 1))
        <*> (unStringDouble <$> parseJSON (a V.! 2))

data Match
    = Match
        { _matchTradeId      :: TradeId
        , _matchMakerOrderId :: UUID
        , _matchTakerOrderId :: UUID
        , _matchProductId    :: ProductId
        , _matchSequence     :: Sequence
        , _matchSide         :: Side
        , _matchSize         :: Double
        , _matchPrice        :: Double
        }

instance FromJSON Match where
    parseJSON = withObject "Match" $ \o -> do
        t <- o .: "type"
        case t of
            "last_match" -> process o
            "match" -> process o
            _ -> fail $ T.unpack $ "Expected type 'subscribe' got '" <> t <> "'."
        where
            process o = Match
                <$> o .: "trade_id"
                <*> o .: "maker_order_id"
                <*> o .: "taker_order_id"
                <*> o .: "product_id"
                <*> o .: "sequence"
                <*> o .: "side"
                <*> (o .: "size" >>= textDouble)
                <*> (o .: "price" >>= textDouble)
