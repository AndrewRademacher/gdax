{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.GDAX.Test.Feed
    ( tests
    ) where

import qualified Data.Aeson              as Aeson
import           Network.GDAX.Test.Types
import           Network.GDAX.Types.Feed
import           Network.WebSockets
import           Test.Tasty
import           Test.Tasty.HUnit
import           Wuss

tests :: Env -> TestTree
tests e = testGroup "Feed Parse"
    [ case_heartbeat e
    , case_ticker e
    ]

case_heartbeat :: Env -> TestTree
case_heartbeat _ = testCase "Heartbeats" $
        runSecureClient "ws-feed.gdax.com" 443 "/" client
    where
        client :: ClientApp ()
        client conn = do
            sendTextData conn (Aeson.encode testSub)

            m1 <- receiveData conn
            m2 <- receiveData conn

            sendTextData conn (Aeson.encode testUnSub)

            let subs = Aeson.eitherDecode m1 :: Either String Subscriptions
                h1 = Aeson.eitherDecode m2 :: Either String Heartbeat

            assertRight subs
            assertRight h1

        testSub = Subscribe $ Subscriptions [] [ChannelSubscription ChannelHeartbeat ["BTC-USD"]]
        testUnSub =  UnSubscribe $ Subscriptions [] [ChannelSubscription ChannelHeartbeat ["BTC-USD"]]

case_ticker :: Env -> TestTree
case_ticker _ = testCase "Ticker" $
        runSecureClient "ws-feed.gdax.com" 443 "/" client
    where
        client :: ClientApp ()
        client conn = do
            sendTextData conn (Aeson.encode testSub)

            -- m1 <- receiveData conn
            m2 <- receiveData conn

            sendTextData conn (Aeson.encode testUnSub)

            -- let subs = Aeson.eitherDecode m1 :: Either String Subscriptions
            let h1 = Aeson.eitherDecode m2 :: Either String Ticker

            -- assertRight subs
            assertRight h1

        testSub = Subscribe $ Subscriptions [] [ChannelSubscription ChannelTicker ["BTC-USD"]]
        testUnSub =  UnSubscribe $ Subscriptions [] [ChannelSubscription ChannelTicker ["BTC-USD"]]

assertRight :: (Show e) => Either e a -> IO ()
assertRight (Right _) = return ()
assertRight (Left er) = fail (show er)
