module Main where

import           Control.Monad.IO.Class
import           Data.Aeson                 (Value)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Network.GDAX.Explicit
import           System.Environment

main :: IO ()
main = do
    gAccessKey <- CBS.pack <$> getEnv "GDAX_KEY"
    gSecretKey <- Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    gPassphrase <- CBS.pack <$> getEnv "GDAX_PASSPHRASE"

    g <- mkSandboxGdax gAccessKey gSecretKey gPassphrase

    putStrLn "getTime:"
    time <- getTime g
    print time

    putStrLn "listAccounts:"
    accounts <- listAccounts g
    CLBS.putStrLn $ encodePretty accounts

    putStrLn "placeOrder:"
    ores <- placeOrder g
    CLBS.putStrLn $ encodePretty ores

withGdax :: (MonadIO m) => (Gdax -> m a) -> m a
withGdax f = do
    gAccessKey <- liftIO $ CBS.pack <$> getEnv "GDAX_KEY"
    gSecretKey <- liftIO $ Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    gPassphrase <- liftIO $ CBS.pack <$> getEnv "GDAX_PASSPHRASE"

    g <- mkSandboxGdax gAccessKey gSecretKey gPassphrase

    f g

printPrettyLn :: (MonadIO m) => Value -> m ()
printPrettyLn = liftIO . CLBS.putStrLn . encodePretty
