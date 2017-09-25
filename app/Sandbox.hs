module Main where

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as CBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Network.GDAX
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
