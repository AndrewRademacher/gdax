{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Data.ByteString.Base64       as Base64
-- import qualified Data.ByteString.Char8        as CBS
import           Network.GDAX.Implicit
import qualified Network.GDAX.Test.MarketData as MarketData
-- import           System.Environment
import           Test.Tasty

main :: IO ()
main = do
    -- gAccessKey <- CBS.pack <$> getEnv "GDAX_KEY"
    -- gSecretKey <- Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SECRET"
    -- gPassphrase <- CBS.pack <$> getEnv "GDAX_PASSPHRASE"

    -- sbox <- mkSandboxGdax gAccessKey gSecretKey gPassphrase
    live <- mkLiveUnsignedGdax
    defaultMain $ tests live

tests :: Gdax -> TestTree
tests g = testGroup "Tests"
    [ MarketData.tests g
    ]
