{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 (Value)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.GDAX.Explicit
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
subscribeSocket = runSecureClient "echo.websocket.org" 443 "/" ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected."

    void . forkIO . forever $ do
        msg <- receiveData connection
        print (msg :: Text)

    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (T.pack line)
                loop
    loop

    sendClose connection (T.pack "Bye!")
