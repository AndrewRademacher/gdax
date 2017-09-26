{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 (Value)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Data.Text                  (Text)
import           Network.GDAX.Explicit
import           Network.GDAX.Types.Feed
import           Network.WebSockets
import           System.Environment
import           Wuss

main :: IO ()
main = putStrLn "For use with GHCi."

withGdax :: (MonadIO m) => (Gdax -> m a) -> m a
withGdax f = do
    gAccessKey <- liftIO $ CBS.pack <$> getEnv "GDAX_KEY"
    gSecretKey <- liftIO $ Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    gPassphrase <- liftIO $ CBS.pack <$> getEnv "GDAX_PASSPHRASE"

    g <- mkSandboxGdax gAccessKey gSecretKey gPassphrase

    f g

printPrettyLn :: (MonadIO m) => Value -> m ()
printPrettyLn = liftIO . CLBS.putStrLn . encodePretty

subscribeSocket :: IO ()
subscribeSocket = runSecureClient "ws-feed.gdax.com" 443 "/" client

testSub :: Subscribe
testSub = Subscribe [] [ChannelSubscription Full ["BTC-USD"]]

client :: ClientApp ()
client conn = do
    putStrLn "Connection opened.."
    sendTextData conn (Aeson.encode testSub)
    void . forever $ do
        msg <- receiveData conn
        print (msg :: Text)
