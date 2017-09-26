{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Network.GDAX.Core
    ( Endpoint
    , AccessKey, SecretKey, Passphrase
    , Path, Method

    , Gdax

    , HasGdax (..)
    , HasNetworkManager (..)
    , HasRestEndpoint (..)
    , HasSocketEndpoint (..)
    , HasAccessKey (..)
    , HasSecretKey (..)
    , HasPassphrase (..)

    , mkLiveGdax, mkSandboxGdax
    , mkLiveUnsignedGdax, mkSandboxUnsignedGdax

    , gdaxGet
    , gdaxGetWith
    , gdaxSignedGet
    , gdaxSignedPost
    ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import           Data.Byteable
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.GDAX.Exceptions
import           Network.HTTP.Client        (Manager)
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Network.Wreq
import           Text.Printf

type Endpoint = String
type AccessKey = ByteString
type SecretKey = ByteString
type Passphrase = ByteString

type Path = String
type Method = ByteString

liveRest :: Endpoint
liveRest = "https://api.gdax.com"

sandboxRest :: Endpoint
sandboxRest = "https://api-public.sandbox.gdax.com"

liveSocket :: Endpoint
liveSocket = "ws-feed.gdax.com"

sandboxSocket :: Endpoint
sandboxSocket = "ws-feed-public.sandbox.gdax.com"

class HasNetworkManager a where
    networkManager :: Lens' a Manager
class HasRestEndpoint a where
    restEndpoint :: Lens' a Endpoint
class HasSocketEndpoint a where
    socketEndpoint :: Lens' a Endpoint
class HasAccessKey a where
    accessKey :: Lens' a AccessKey
class HasSecretKey a where
    secretKey :: Lens' a SecretKey
class HasPassphrase a where
    passphrase :: Lens' a Passphrase

data Gdax
    = Gdax
        { _gdaxNetworkManager :: Manager
        , _gdaxRestEndpoint   :: Endpoint
        , _gdaxSocketEndpoint :: Endpoint
        , _gdaxAccessKey      :: AccessKey
        , _gdaxSecretKey      :: SecretKey
        , _gdaxPassphrase     :: Passphrase
        }

$(makeClassy ''Gdax)

instance HasNetworkManager Gdax where networkManager = gdaxNetworkManager
instance HasRestEndpoint Gdax where restEndpoint = gdaxRestEndpoint
instance HasSocketEndpoint Gdax where socketEndpoint = gdaxSocketEndpoint
instance HasAccessKey Gdax where accessKey = gdaxAccessKey
instance HasSecretKey Gdax where secretKey = gdaxSecretKey
instance HasPassphrase Gdax where passphrase = gdaxPassphrase

mkLiveGdax :: (MonadIO m) => AccessKey -> SecretKey -> Passphrase -> m Gdax
mkLiveGdax a s p = do
    m <- newTlsManager
    return $ Gdax m liveRest liveSocket a s p

mkSandboxGdax :: (MonadIO m) => AccessKey -> SecretKey -> Passphrase -> m Gdax
mkSandboxGdax a s p = do
    m <- newTlsManager
    return $ Gdax m sandboxRest sandboxSocket a s p

mkLiveUnsignedGdax :: (MonadIO m) => m Gdax
mkLiveUnsignedGdax = do
    m <- newTlsManager
    return $ Gdax m liveRest liveSocket "" "" ""

mkSandboxUnsignedGdax :: (MonadIO m) => m Gdax
mkSandboxUnsignedGdax = do
    m <- newTlsManager
    return $ Gdax m sandboxRest sandboxSocket "" "" ""

gdaxGet :: (MonadIO m, MonadThrow m, FromJSON b) => Gdax -> Path -> m b
{-# INLINE gdaxGet #-}
gdaxGet g path = do
    res <- liftIO $ getWith opts (g ^. restEndpoint <> path)
    decodeResult res
    where
        opts = defaults & manager .~ Right (g ^. networkManager)

gdaxGetWith :: (MonadIO m, MonadThrow m, FromJSON b) => Gdax -> Path -> Options -> m b
{-# INLINE gdaxGetWith #-}
gdaxGetWith g path opts' = do
    res <- liftIO $ getWith opts (g ^. restEndpoint <> path)
    decodeResult res
    where
        opts = opts' & manager .~ Right (g ^. networkManager)

gdaxSignedGet :: (MonadIO m, MonadThrow m, FromJSON b) => Gdax -> Path -> m b
{-# INLINE gdaxSignedGet #-}
gdaxSignedGet g path = do
    signedOpts <- signOptions g "GET" path Nothing opts
    res <- liftIO $ getWith signedOpts (g ^. restEndpoint <> path)
    decodeResult res
    where
        opts = defaults & manager .~ Right (g ^. networkManager)

gdaxSignedPost :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) => Gdax -> Path -> a -> m b
{-# INLINE gdaxSignedPost #-}
gdaxSignedPost g path body = do
    signedOpts <- signOptions g "POST" path (Just bodyBS) opts
    res <- liftIO $ postWith signedOpts (g ^. restEndpoint <> path) bodyBS
    decodeResult res
    where
        opts = defaults & header "Content-Type" .~ [ "application/json" ]
                        & manager .~ Right (g ^. networkManager)
        bodyBS = CLBS.toStrict $ Aeson.encode body

decodeResult :: (MonadThrow m, FromJSON a) => Response CLBS.ByteString -> m a
{-# INLINE decodeResult #-}
decodeResult res =
    case Aeson.eitherDecode' (res ^. responseBody) of
        Left err  -> throwM $ MalformedGDAXResponse (T.pack err)
        Right val -> return val

signOptions :: (MonadIO m) => Gdax -> Method -> Path -> (Maybe ByteString) -> Options -> m Options
{-# INLINE signOptions #-}
signOptions g method path mBody opts = do
    time <- liftIO $ getCurrentTime
    let timestamp = CBS.pack $ printf "%.0f" (realToFrac (utcTimeToPOSIXSeconds time) :: Double)
        sigString = timestamp <> method <> (CBS.pack path) <> maybe "" id mBody
        sig = Base64.encode $ toBytes (hmac (g ^. secretKey) sigString :: HMAC SHA256)

    return $ opts
        & header "CB-ACCESS-KEY" .~ [ (g ^. accessKey) ]
        & header "CB-ACCESS-SIGN" .~ [ sig ]
        & header "CB-ACCESS-TIMESTAMP" .~ [ timestamp ]
        & header "CB-ACCESS-PASSPHRASE" .~ [ (g ^. passphrase) ]
