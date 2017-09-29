{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.GDAX.Test.Feed
    ( tests
    ) where

import           Control.Lens
import           Data.Aeson                (FromJSON (..))
import           Data.Aeson                (Value (..))
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy      as LBS
import           Data.Proxy
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Vector               (Vector)
import           Network.GDAX.Core
import           Network.GDAX.Test.Types
import           Network.GDAX.Types.Feed
import           Network.GDAX.Types.Shared
import           Network.WebSockets
import           Test.Tasty
import           Test.Tasty.HUnit
import           Wuss

tests :: Env -> TestTree
tests e = testGroup "feed"
    [ parseTestClient e (mkBTCSub [ChannelHeartbeat]) "heartbeat" (Proxy :: Proxy Heartbeat)
    , parseTestClient e (mkBTCSub [ChannelTicker]) "ticker" (Proxy :: Proxy Ticker)
    , parseTestClient e (mkBTCSub [ChannelLevel2]) "snapshot" (Proxy :: Proxy Level2Snapshot)
    , parseTestClient e (mkBTCSub [ChannelLevel2]) "l2update" (Proxy :: Proxy Level2Update)
    , parseTestClient e (mkBTCSub [ChannelMatches]) "last_match" (Proxy :: Proxy Match)
    , parseTestClient e (mkBTCSub [ChannelMatches]) "match" (Proxy :: Proxy Match)
    , parseTestClient e (mkBTCSub [ChannelFull]) "received" (Proxy :: Proxy Received)
    , parseTestClient e (mkBTCSub [ChannelFull]) "open" (Proxy :: Proxy Open)
    , parseTestClient e (mkBTCSub [ChannelFull]) "done" (Proxy :: Proxy Done)
    , parseTestClient e (mkBTCSub [ChannelFull]) "match" (Proxy :: Proxy Match)

    -- This one cannot run independently, since you only receive them if you
    -- trade against yourself.
    -- , parseTestClient (mkBTCSub [ChannelFull]) "change" (Proxy :: Proxy Change)

    -- This one cannot run independently, since you have to be authenticated with
    -- a margin profile.
    -- , parseTestClient (mkBTCSub [ChannelFull]) "margin_profile_update" (Proxy :: Proxy MarginProfileUpdate)

    -- This one cannot run independently, they occur very infrequently.
    -- , parseTestClient (mkBTCSub [ChannelFull]) "activate" (Proxy :: Proxy Activate)

    , case_sum e
    ]

mkBTCSub :: Vector Channel -> Subscriptions
mkBTCSub = mkSubscriptions "BTC-USD"

mkSubscriptions :: ProductId -> Vector Channel -> Subscriptions
mkSubscriptions pid cs = Subscriptions [] $ fmap fn cs
    where
        fn c = ChannelSubscription c [pid]

parseTestClient :: (FromJSON a) => Env -> Subscriptions -> Text -> Proxy a -> TestTree
parseTestClient e subs t pt = testCase (T.unpack t) $ runSecureClient (e ^. liveUnsigned . socketEndpoint) 443 "/" $ \conn -> do
    sendTextData conn (Aeson.encode testSub)
    m1 <- receiveOfType conn t
    let res = Aeson.eitherDecode m1
    case res of
        Left er -> fail (show er)
        Right v ->
            let _final = asProxyTypeOf v pt
            in return ()

    sendTextData conn (Aeson.encode testUnSub)
    where
        testSub = Subscribe subs
        testUnSub =  UnSubscribe subs

case_sum :: Env -> TestTree
case_sum _ = testCase "sum" $
        runSecureClient "ws-feed.gdax.com" 443 "/" client
    where
        client :: ClientApp ()
        client conn = do
            sendTextData conn (Aeson.encode testSub)

            ms <- sequence $ take 100 $ repeat (receiveNotSubs conn)

            sendTextData conn (Aeson.encode testUnSub)

            let res = fmap Aeson.eitherDecode ms

            mapM_ assertRight (res :: [Either String GdaxMessage])

        testSub = Subscribe $ Subscriptions [] subs
        testUnSub =  UnSubscribe $ Subscriptions [] subs
        subs =
            [ ChannelSubscription ChannelHeartbeat ["BTC-USD"]
            , ChannelSubscription ChannelTicker ["BTC-USD"]
            , ChannelSubscription ChannelLevel2 ["BTC-USD"]
            , ChannelSubscription ChannelMatches ["BTC-USD"]
            ]

receiveOfType :: Connection -> Text -> IO LBS.ByteString
receiveOfType conn t = receiveOfTypes conn [t]

receiveOfTypes :: Connection -> [Text] -> IO LBS.ByteString
receiveOfTypes conn ts = loop
    where
        tset = Set.fromList ts
        loop = do
            res <- receiveData conn
            let asValue = Aeson.eitherDecode res :: Either String Value
            case asValue of
                Left er -> fail (show er)
                Right v ->
                    case v ^? key "type" . _String of
                        Nothing -> loop
                        Just t ->
                            if Set.member t tset
                                then return res
                                else loop

receiveNotSubs :: Connection -> IO LBS.ByteString
receiveNotSubs conn = loop
    where
        loop = do
            res <- receiveData conn
            let asValue = Aeson.eitherDecode res :: Either String Value
            case asValue of
                Left er -> fail (show er)
                Right v ->
                    if (v ^? key "type") == (Just (String "subscriptions"))
                        then loop
                        else return res

assertRight :: (Show e) => Either e a -> IO ()
assertRight (Right _) = return ()
assertRight (Left er) = fail (show er)
