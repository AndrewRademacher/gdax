{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as CBS
import           Network.GDAX.Implicit
import qualified Network.GDAX.Test.Feed       as Feed
import qualified Network.GDAX.Test.MarketData as MarketData
import qualified Network.GDAX.Test.Private    as Private
import           Network.GDAX.Test.Types
import           System.Environment
import           Test.Tasty

main :: IO ()
main = do
    gAccessKey <- CBS.pack <$> getEnv "GDAX_SANDBOX_KEY"
    gSecretKey <- Base64.decodeLenient . CBS.pack <$> getEnv "GDAX_SANDBOX_SECRET"
    gPassphrase <- CBS.pack <$> getEnv "GDAX_SANDBOX_PASSPHRASE"

    sbox <- mkSandboxGdax gAccessKey gSecretKey gPassphrase
    live <- mkLiveUnsignedGdax
    defaultMain $ tests (Env sbox live)

tests :: Env -> TestTree
tests e = testGroup "Tests"
    [ MarketData.tests e
    , Private.tests e
    , Feed.tests e
    ]
