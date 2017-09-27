{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.Feed where

import           Data.Aeson
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time
import           Data.Typeable
import           Data.UUID
import           Data.Vector
import qualified Data.Vector.Generic       as V
import           GHC.Generics
import           Network.GDAX.Parsers
import           Network.GDAX.Types.Shared

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
        , _matchTime         :: Maybe UTCTime
        , _matchMakerOrderId :: UUID
        , _matchTakerOrderId :: UUID
        , _matchProductId    :: ProductId
        , _matchSequence     :: Sequence
        , _matchSide         :: Side
        , _matchSize         :: Double
        , _matchPrice        :: Double
        }
    deriving (Show, Typeable, Generic)

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
                <*> o .:? "time"
                <*> o .: "maker_order_id"
                <*> o .: "taker_order_id"
                <*> o .: "product_id"
                <*> o .: "sequence"
                <*> o .: "side"
                <*> (o .: "size" >>= textDouble)
                <*> (o .: "price" >>= textDouble)

-- Full Book Messages

data Received
    = ReceivedLimit
        { _receivedTime      :: UTCTime
        , _receivedProductId :: ProductId
        , _receivedSequence  :: Sequence
        , _receivedOrderId   :: OrderId
        , _receivedSize      :: Double
        , _receivedPrice     :: Double
        , _receivedSide      :: Side
        }
    | ReceivedMarket
        { _receivedTime      :: UTCTime
        , _receivedProductId :: ProductId
        , _receivedSequence  :: Sequence
        , _receivedOrderId   :: OrderId
        , _receivedFunds     :: Double
        , _receivedSide      :: Side
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Received where
    parseJSON = withObjectOfType "Received" "received" $ \o -> do
        t <- o .: "order_type"
        case t of
            OrderLimit -> ReceivedLimit
                <$> o .: "time"
                <*> o .: "product_id"
                <*> o .: "sequence"
                <*> o .: "order_id"
                <*> (o .: "size" >>= textDouble)
                <*> (o .: "price" >>= textDouble)
                <*> o .: "side"
            OrderMarket -> ReceivedMarket
                <$> o .: "time"
                <*> o .: "product_id"
                <*> o .: "sequence"
                <*> o .: "order_id"
                <*> (o .: "funds" >>= textDouble)
                <*> o .: "side"

data Reason
    = ReasonFilled
    | ReasonCanceled
    deriving (Eq, Ord, Typeable, Generic)

instance Show Reason where
    show ReasonFilled   = "filled"
    show ReasonCanceled = "canceled"

instance FromJSON Reason where
    parseJSON = withText "Reason" $ \t ->
        case t of
            "filled"   -> pure ReasonFilled
            "canceled" -> pure ReasonCanceled
            _          -> fail $ T.unpack $ "'" <> t <> "' is not a valid reason."

data Open
    = Open
        { _openTime          :: UTCTime
        , _openProductId     :: ProductId
        , _openOrderId       :: OrderId
        , _openSequence      :: Sequence
        , _openPrice         :: Double
        , _openRemainingSize :: Double
        , _openSide          :: Side
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Open where
    parseJSON = withObjectOfType "Open" "open" $ \o -> Open
        <$> o .: "time"
        <*> o .: "product_id"
        <*> o .: "order_id"
        <*> o .: "sequence"
        <*> (o .: "price" >>= textDouble)
        <*> (o .: "remaining_size" >>= textDouble)
        <*> o .: "side"

data Done
    = Done
        { _doneTime          :: UTCTime
        , _doneProductId     :: ProductId
        , _doneSequence      :: Sequence
        , _donePrice         :: Maybe Double
        , _doneOrderId       :: OrderId
        , _doneReason        :: Reason
        , _doneSide          :: Side
        , _doneRemainingSize :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Done where
    parseJSON = withObjectOfType "Done" "done" $ \o -> Done
        <$> o .: "time"
        <*> o .: "product_id"
        <*> o .: "sequence"
        <*> (o .:? "price" >>= textMaybeDouble)
        <*> o .: "order_id"
        <*> o .: "reason"
        <*> o .: "side"
        <*> (o .: "remaining_size" >>= textDouble)

-- Match implemented previously

data Change
    = ChangeSize
        { _changeTime      :: UTCTime
        , _changeSequence  :: Sequence
        , _changeOrderId   :: OrderId
        , _changeProductId :: ProductId
        , _changeNewSize   :: Double
        , _changeOldSize   :: Double
        , _changePrice     :: Double
        , _changSide       :: Side
        }
    | ChangeFunds
        { _changeTime      :: UTCTime
        , _changeSequence  :: Sequence
        , _changeOrderId   :: OrderId
        , _changeProductId :: ProductId
        , _changeNewFunds  :: Double
        , _changeOldFunds  :: Double
        , _changePrice     :: Double
        , _changSide       :: Side
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Change where
    parseJSON = withObjectOfType "Change" "change" $ \o -> do
        fund <- o .:? "new_funds"
        case (fund :: Maybe Double) of
            Nothing -> ChangeSize
                <$> o .: "time"
                <*> o .: "sequence"
                <*> o .: "order_id"
                <*> o .: "product_id"
                <*> o .: "new_size"
                <*> o .: "old_size"
                <*> o .: "price"
                <*> o .: "side"
            Just _ -> ChangeFunds
                <$> o .: "time"
                <*> o .: "sequence"
                <*> o .: "order_id"
                <*> o .: "product_id"
                <*> o .: "new_funds"
                <*> o .: "old_funds"
                <*> o .: "price"
                <*> o .: "side"

data MarginProfileUpdate
    = MarginProfileUpdate
        { _mpuProductId          :: ProductId
        , _mpuTime               :: UTCTime
        , _mpuUserId             :: UserId
        , _mpuProfileId          :: ProfileId
        , _mpuNonce              :: Int
        , _mpuPosition           :: Text
        , _mpuPositionSize       :: Double
        , _mpuPositionCompliment :: Double
        , _mpuPositionMaxSize    :: Double
        , _mpuCallSide           :: Side
        , _mpuCallPrice          :: Double
        , _mpuCallSize           :: Double
        , _mpuCallFunds          :: Double
        , _mpuCovered            :: Bool
        , _mpuNextExpireTime     :: UTCTime
        , _mpuBaseBalance        :: Double
        , _mpuBaseFunding        :: Double
        , _mpuQuoteBalance       :: Double
        , _mpuQuoteFunding       :: Double
        , _mpuPrivate            :: Bool
        }
    deriving (Show, Typeable, Generic)

instance FromJSON MarginProfileUpdate where
    parseJSON = withObjectOfType "MarginProfileUpdate" "margin_profile_update" $ \o -> MarginProfileUpdate
        <$> o .: "product_id"
        <*> o .: "timestamp"
        <*> o .: "user_id"
        <*> o .: "profile_id"
        <*> o .: "nonce"
        <*> o .: "position"
        <*> (o .: "position_size" >>= textDouble)
        <*> (o .: "position_compliement" >>= textDouble)
        <*> (o .: "position_max_size" >>= textDouble)
        <*> o .: "call_side"
        <*> (o .: "call_price" >>= textDouble)
        <*> (o .: "call_size" >>= textDouble)
        <*> (o .: "call_funds" >>= textDouble)
        <*> o .: "covered"
        <*> o .: "next_expire_time"
        <*> (o .: "base_balance" >>= textDouble)
        <*> (o .: "base_funding" >>= textDouble)
        <*> (o .: "quote_balance" >>= textDouble)
        <*> (o .: "quote_funding" >>= textDouble)
        <*> o .: "private"

data Activate
    = Activate
        { _activateProductId    :: ProductId
        , _activateTime         :: UTCTime
        , _activateUserId       :: UserId
        , _activateProfileId    :: ProfileId
        , _activateOrderId      :: OrderId
        , _activateStopType     :: StopType
        , _activateSide         :: Side
        , _activateStopPrice    :: Double
        , _activateSize         :: Double
        , _activateFunds        :: Double
        , _activateTakerFeeRate :: Double
        , _activatePrivate      :: Bool
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Activate where
    parseJSON = withObjectOfType "Activate" "activate" $ \o -> Activate
        <$> o .: "product_id"
        <*> o .: "time"
        <*> o .: "user_id"
        <*> o .: "profile_id"
        <*> o .: "order_id"
        <*> o .: "stop_type"
        <*> o .: "side"
        <*> (o .: "stop_price" >>= textDouble)
        <*> (o .: "size" >>= textDouble)
        <*> (o .: "funds" >>= textDouble)
        <*> (o .: "taker_fee_rate" >>= textDouble)
        <*> o .: "private"

-- Sum Type

data GdaxMessage
    = GdaxSubscriptions Subscriptions
    | GdaxHeartbeat Heartbeat
    | GdaxTicker Ticker
    | GdaxLevel2Snapshot Level2Snapshot
    | GdaxLevel2Update Level2Update
    | GdaxMatch Match
    | GdaxReceived Received
    | GdaxOpen Open
    | GdaxDone Done
    | GdaxChange Change
    | GdaxMarginProfileUpdate MarginProfileUpdate
    | GdaxActivate Activate
    | GdaxFeedError FeedError
    deriving (Show, Typeable, Generic)

instance FromJSON GdaxMessage where
    parseJSON = withObject "GdaxMessage" $ \o -> do
        t <- o .: "type"
        case t of
            "subscriptions" -> GdaxSubscriptions <$> parseJSON (Object o)
            "heartbeat" -> GdaxHeartbeat <$> parseJSON (Object o)
            "ticker" -> GdaxTicker <$> parseJSON (Object o)
            "snapshot" -> GdaxLevel2Snapshot <$> parseJSON (Object o)
            "l2update" -> GdaxLevel2Update <$> parseJSON (Object o)
            "last_match" -> GdaxMatch <$> parseJSON (Object o)
            "match" -> GdaxMatch <$> parseJSON (Object o)
            "received" -> GdaxReceived <$> parseJSON (Object o)
            "open" -> GdaxOpen <$> parseJSON (Object o)
            "done" -> GdaxDone <$> parseJSON (Object o)
            "change" -> GdaxChange <$> parseJSON (Object o)
            "margin_profile_update" -> GdaxMarginProfileUpdate <$> parseJSON (Object o)
            "activate" -> GdaxActivate <$> parseJSON (Object o)
            "error" -> GdaxFeedError <$> parseJSON (Object o)
            _ -> fail $ T.unpack $ "Message of unsupported type '" <> t <> "'."
