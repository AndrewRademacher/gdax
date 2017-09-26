{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GDAX.Test.MarketData
    ( tests
    ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Time
import           Network.GDAX.Implicit
import           Network.GDAX.Test.Types
import           Test.Tasty
import           Test.Tasty.HUnit

data LocalEnv
    = LocalEnv
        { _localGdax :: Gdax
        }

$(makeClassy ''LocalEnv)

instance HasGdax LocalEnv where gdax = localGdax

--------------------------------
-- NOTE: ["case_parse" test cases]
--
-- The 'case_parse' function does NOT test that the API responses are parsed correctly
-- For example, the price can be $240 be parsed as $420 and the test will succeed.
-- The function only tests that the parser did not fail and returned *a value*
-- Whether the value returned is the correct one, that's a different matter, and
-- 'case-parse' is NOT testing that.
--
--------------------------------

tests :: Env -> TestTree
tests e = testGroup "MarketData Parse"
    [ case_parse l "getProducts"            $ getProducts
    , case_parse l "getProductTopOfBook"    $ getProductTopOfBook     defProduct
    , case_parse l "getProductTop50OfBook"  $ getProductTop50OfBook   defProduct
    , case_parse l "getProductOrderBook"    $ getProductOrderBook     defProduct
    , case_parse l "getProductTicker"       $ getProductTicker        defProduct
    , case_parse l "getProductTrades"       $ getProductTrades        defProduct
    , case_parse l "getProductHistory"      $ getProductHistory       defProduct defStart defEnd (Just 3600)
    , case_parse l "getCurrencies"          $ getCurrencies
    , case_parse l "getExchangeTime"        $ getTime
    ]
    where
        l = e ^. liveUnsigned

defProduct :: ProductId
defProduct = ProductId "BTC-USD"

-- The date range below works well to provide data for the getHistory call in the sandboxed environment
-- The previous range yielded an empty list with no data when connected to the sandbox.
defStart :: Maybe UTCTime
defStart = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-10-01T20:22:37+0000"

defEnd :: Maybe UTCTime
defEnd = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-10-07T21:22:37+0000"

-- See NOTE: ["case_parse" test cases]
case_parse :: (Show a) => Gdax -> String -> ReaderT LocalEnv IO a -> TestTree
case_parse g l fn = testCase l $ runReaderT (void fn) (LocalEnv g)
