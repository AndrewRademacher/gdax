{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Lens               hiding ((.=))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Crypto.Hash
import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             object, (.=))
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import           Data.Byteable
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Wreq
import           System.Environment
import           Text.Printf


type Endpoint = String
type AccessKey = ByteString
type SecretKey = ByteString
type Passphrase = ByteString

live :: Endpoint
live = "https://api.gdax.com"

sandbox :: Endpoint
sandbox = "https://api-public.sandbox.gdax.com"

class HasEndpoint a where
    endpoint :: Lens' a Endpoint
class HasAccessKey a where
    accessKey :: Lens' a AccessKey
class HasSecretKey a where
    secretKey :: Lens' a SecretKey
class HasPassphrase a where
    passphrase :: Lens' a Passphrase

data GDAX
    = GDAX
        { _gdaxEndpoint   :: Endpoint
        , _gdaxAccessKey  :: AccessKey
        , _gdaxSecretKey  :: SecretKey
        , _gdaxPassphrase :: Passphrase
        }
    deriving (Show)

$(makeClassy ''GDAX)

instance HasEndpoint GDAX where endpoint = gdaxEndpoint
instance HasAccessKey GDAX where accessKey = gdaxAccessKey
instance HasSecretKey GDAX where secretKey = gdaxSecretKey
instance HasPassphrase GDAX where passphrase = gdaxPassphrase

mkLiveGDAX :: AccessKey -> SecretKey -> Passphrase -> GDAX
mkLiveGDAX = GDAX live

mkSandboxGDAX :: AccessKey -> SecretKey -> Passphrase -> GDAX
mkSandboxGDAX = GDAX sandbox

mkLiveUnsignedGDAX :: GDAX
mkLiveUnsignedGDAX = GDAX live "" "" ""

mkSandboxUnsignedGDAX :: GDAX
mkSandboxUnsignedGDAX = GDAX sandbox "" "" ""

main :: IO ()
main = do
    accessKey <- liftIO $ CBS.pack <$> getEnv "GDAX_KEY"
    secretKey <- liftIO $ Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    passphrase <- liftIO $ CBS.pack <$> getEnv "GDAX_PASSPHRASE"

    let gdax = mkSandboxGDAX accessKey secretKey passphrase

    putStrLn "getTime:"
    time <- getTime gdax
    print time

    putStrLn "listAccounts:"
    accounts <- listAccounts gdax
    CLBS.putStrLn $ encodePretty accounts

    putStrLn "placeOrder:"
    ores <- placeOrder gdax
    CLBS.putStrLn $ encodePretty ores

-- BEGIN: General Functions

type Path = String
type Method = ByteString

gdaxGet :: (MonadIO m, MonadThrow m, FromJSON b) => GDAX -> Path -> m b
gdaxGet gdax path = do
    res <- liftIO $ get (gdax ^. endpoint <> path)
    case Aeson.decode (res ^. responseBody) of
        Nothing -> throwM $ MalformedGDAXResponse "Could not parse GDAX response body."
        Just val -> return val

gdaxSignedGet :: (MonadIO m, MonadThrow m, FromJSON b) => GDAX -> Path -> m b
gdaxSignedGet gdax path = do
    signedOpts <- signOptions gdax "GET" path Nothing defaults
    res <- liftIO $ getWith signedOpts (gdax ^. endpoint <> path)
    case Aeson.decode (res ^. responseBody) of
        Nothing -> throwM $ MalformedGDAXResponse "Could not parse GDAX response body."
        Just val -> return val

gdaxSignedPost :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) => GDAX -> Path -> a -> m b
gdaxSignedPost gdax path body = do
    signedOpts <- signOptions gdax "POST" path (Just bodyBS) opts
    res <- liftIO $ postWith signedOpts (gdax ^. endpoint <> path) bodyBS
    case Aeson.decode (res ^. responseBody) of
        Nothing -> throwM $ MalformedGDAXResponse "Could not parse GDAX response body."
        Just val -> return val
    where
        opts = defaults & header "Content-Type" .~ [ "application/json" ]
        bodyBS = CLBS.toStrict $ Aeson.encode body

-- | No export
signOptions :: (MonadIO m) => GDAX -> Method -> Path -> (Maybe ByteString) -> Options -> m Options
signOptions gdax method path mBody opts = do
    time <- liftIO $ getCurrentTime
    let timestamp = CBS.pack $ printf "%.0f" (realToFrac (utcTimeToPOSIXSeconds time) :: Double)
        sigString = timestamp <> method <> (CBS.pack path) <> maybe "" id mBody
        sig = Base64.encode $ toBytes (hmac (gdax ^. secretKey) sigString :: HMAC SHA256)

    return $ opts
        & header "CB-ACCESS-KEY" .~ [ (gdax ^. accessKey) ]
        & header "CB-ACCESS-SIGN" .~ [ sig ]
        & header "CB-ACCESS-TIMESTAMP" .~ [ timestamp ]
        & header "CB-ACCESS-PASSPHRASE" .~ [ (gdax ^. passphrase) ]

-- END: General Functions

-- placeOrder - Example of making authenticated request with body against GDAX.

placeOrder' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGDAX e) => m Value
placeOrder' = do
    gdax <- (^. gDAX) <$> ask
    placeOrder gdax

placeOrder :: (MonadIO m, MonadThrow m) => GDAX -> m Value
placeOrder gdax = gdaxSignedPost gdax "/orders" body
    where
        body = object
            [ "size" .= ("0.01" :: Text)
            , "price" .= ("0.100" :: Text)
            , "side" .= ("buy" :: Text)
            , "product_id" .= ("BTC-USD" :: Text)
            ]

-- listAccounts - Example of making an authenticated request against GDAX.

listAccounts' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGDAX e) => m Value
listAccounts' = do
    gdax <- (^. gDAX) <$> ask
    listAccounts gdax

listAccounts :: (MonadIO m, MonadThrow m) => GDAX -> m Value
listAccounts gdax =
    gdaxSignedGet gdax "/accounts"

-- getTime - Example of making an unauthenticated request against GDAX.

getTime' :: (MonadIO m, MonadThrow m, MonadReader e m, HasGDAX e) => m UTCTime
getTime' = do
    gdax <- (^. gDAX) <$> ask
    getTime gdax

getTime :: (MonadIO m, MonadThrow m) => GDAX -> m UTCTime
getTime gdax = do
    res <- gdaxGet gdax "/time"
    case (res :: Value) ^? key "epoch" . _Double of
        Nothing  -> throwM $ MalformedGDAXResponse "Epoch field was either missing or malformed in response from GET /time."
        Just val -> return $ posixSecondsToUTCTime $ realToFrac val

-- Errors

data MalformedGDAXResponse
    = MalformedGDAXResponse Text
    deriving (Show)

instance Exception MalformedGDAXResponse
