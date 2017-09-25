{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.GDAX.Types.MarketData where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Int
import           Data.Text             (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Data.UUID
import           Data.Vector           (Vector)
import qualified Data.Vector.Generic   as V
import           GHC.Generics
import           Network.GDAX.Parsers

-- Product

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show ProductId where
    show = show . unProductId

data Product
    = Product
        { _prodId             :: ProductId
        , _prodBaseCurrency   :: CurrencyId
        , _prodQuoteCurrency  :: CurrencyId
        , _prodBaseMinSize    :: Double
        , _prodBaseMaxSize    :: Double
        , _prodQuoteIncrement :: Double
        , _prodDisplayName    :: Text
        , _prodMarginEnabled  :: Bool
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Product where
    parseJSON = withObject "Product" $ \o -> Product
        <$> o .: "id"
        <*> o .: "base_currency"
        <*> o .: "quote_currency"
        <*> (o .: "base_min_size" >>= textDouble)
        <*> (o .: "base_max_size" >>= textDouble)
        <*> (o .: "quote_increment" >>= textDouble)
        <*> o .: "display_name"
        <*> o .: "margin_enabled"

-- Book

newtype Sequence = Sequence { unSequence :: Int64 }
    deriving (Eq, Ord, Enum, Typeable, Generic, ToJSON, FromJSON)

instance Show Sequence where
    show = show . unSequence

data AggrigateBid
    = AggrigateBid
        { _aggbidPrice      :: {-# UNPACK #-} Double
        , _aggbidSize       :: {-# UNPACK #-} Double
        , _aggbidOrderCount :: {-# UNPACK #-} Int64
        }
    deriving (Show, Typeable, Generic)

instance FromJSON AggrigateBid where
    parseJSON = bookItem "AggrigateBid" AggrigateBid

data AggrigateAsk
    = AggrigateAsk
        { _aggaskPrice      :: {-# UNPACK #-} Double
        , _aggaskSize       :: {-# UNPACK #-} Double
        , _aggaskOrderCount :: {-# UNPACK #-} Int64
        }
    deriving (Show, Typeable, Generic)

instance FromJSON AggrigateAsk where
    parseJSON = bookItem "AggrigateAsk" AggrigateAsk

data Bid
    = Bid
        { _bidPrice :: {-# UNPACK #-} Double
        , _bidSize  :: {-# UNPACK #-} Double
        , _bidId    :: {-# UNPACK #-} UUID
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Bid where
    parseJSON = bookItem "Bid" Bid

data Ask
    = Ask
        { _askPrice :: {-# UNPACK #-} Double
        , _askSize  :: {-# UNPACK #-} Double
        , _askId    :: {-# UNPACK #-} UUID
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Ask where
    parseJSON = bookItem "Ask" Ask

data AggrigateBook
    = AggrigateBook
        { _aggbookBids     :: Vector AggrigateBid
        , _aggbookAsks     :: Vector AggrigateAsk
        , _aggbookSequence :: Sequence
        }
    deriving (Show, Typeable, Generic)

instance FromJSON AggrigateBook where
    parseJSON = withObject "AggrigateBook" $ \o -> AggrigateBook
        <$> o .: "bids"
        <*> o .: "asks"
        <*> o .: "sequence"

data Book
    = Book
        { _bookBids     :: Vector Bid
        , _bookAsks     :: Vector Ask
        , _bookSequence :: Sequence
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Book where
    parseJSON = withObject "Book" $ \o -> Book
        <$> o .: "bids"
        <*> o .: "asks"
        <*> o .: "sequence"

-- Ticker

data Tick
    = Tick
        { _tickTradeId :: TradeId
        , _tickPrice   :: Double
        , _tickSize    :: Double
        , _tickBid     :: Double
        , _tickAsk     :: Double
        , _tickVolume  :: Double
        , _tickTime    :: UTCTime
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Tick where
    parseJSON = withObject "Tick" $ \o -> Tick
        <$> o .: "trade_id"
        <*> (o .: "price" >>= textDouble)
        <*> (o .: "size" >>= textDouble)
        <*> (o .: "bid" >>= textDouble)
        <*> (o .: "ask" >>= textDouble)
        <*> o .: "volume"
        <*> o .: "time"

-- Trade

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

data Trade
    = Trade
        { _tradeId    :: TradeId
        , _tradeTime  :: UTCTime
        , _tradePrice :: Double
        , _tradeSize  :: Double
        , _tradeSide  :: Side
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Trade where
    parseJSON = withObject "Trade" $ \o -> Trade
        <$> o .: "trade_id"
        <*> o .: "time"
        <*> (o .: "price" >>= textDouble)
        <*> (o .: "size" >>= textDouble)
        <*> o .: "side"

-- Candles

type StartTime = UTCTime
type EndTime = UTCTime
type Granularity = Int

type Low = Double
type High = Double
type Open = Double
type Close = Double
type Volume = Double

data Candle = Candle UTCTime Low High Open Close Volume
    deriving (Show, Typeable, Generic)

instance FromJSON Candle where
    parseJSON = withArray "Candle" $ \v ->
        case V.length v of
            6 -> Candle <$> ((posixSecondsToUTCTime . fromIntegral) <$> (parseJSON (v V.! 0) :: Parser Int64))
                <*> parseJSON (v V.! 1)
                <*> parseJSON (v V.! 2)
                <*> parseJSON (v V.! 3)
                <*> parseJSON (v V.! 4)
                <*> parseJSON (v V.! 5)
            _ -> fail "Candle array was not 6 elements wide."

-- Stats

data Stats
    = Stats
        { _statsOpen        :: Open
        , _statsHigh        :: High
        , _statsLow         :: Low
        , _statsVolume      :: Volume
        , _statsVolume30Day :: Volume
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Stats where
    parseJSON = withObject "Stats" $ \o -> Stats
        <$> o .: "open"
        <*> o .: "high"
        <*> o .: "low"
        <*> o .: "volume"
        <*> o .: "volume_30day"

-- Currency

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Typeable, Generic, ToJSON, FromJSON)

instance Show CurrencyId where
    show = show . unCurrencyId

data Currency
    = Currency
        { _currId      :: CurrencyId
        , _currName    :: Text
        , _currMinSize :: Double
        }
    deriving (Show, Typeable, Generic)

instance FromJSON Currency where
    parseJSON = withObject "Currency" $ \o -> Currency
        <$> o .: "id"
        <*> o .: "name"
        <*> (o .: "min_size" >>= textDouble)
