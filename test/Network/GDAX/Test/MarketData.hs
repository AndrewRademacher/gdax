{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GDAX.Test.MarketData
    ( tests
    ) where

import           Control.Monad.Reader
import           Data.Time
import           Network.GDAX.Implicit
import           Network.GDAX.Test.Types
import           Test.Tasty
import           Test.Tasty.HUnit

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

tests :: Gdax -> TestTree
tests g = testGroup "MarketData Parse"
    [ case_parse g "getProducts"            $ getProducts
    , case_parse g "getProductTopOfBook"    $ getProductTopOfBook     defProduct
    , case_parse g "getProductTop50OfBook"  $ getProductTop50OfBook   defProduct
    , case_parse g "getProductOrderBook"    $ getProductOrderBook     defProduct
    , case_parse g "getProductTicker"       $ getProductTicker        defProduct
    , case_parse g "getProductTrades"       $ getProductTrades        defProduct
    , case_parse g "getProductHistory"      $ getProductHistory       defProduct defStart defEnd (Just 3600)
    , case_parse g "getCurrencies"          $ getCurrencies
    , case_parse g "getExchangeTime"        $ getTime
    ]

defProduct :: ProductId
defProduct = ProductId "BTC-USD"

-- The date range below works well to provide data for the getHistory call in the sandboxed environment
-- The previous range yielded an empty list with no data when connected to the sandbox.
defStart :: Maybe UTCTime
defStart = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-10-01T20:22:37+0000"

defEnd :: Maybe UTCTime
defEnd = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-10-07T21:22:37+0000"

-- See NOTE: ["case_parse" test cases]
case_parse :: (Show a) => Gdax -> String -> ReaderT Env IO a -> TestTree
case_parse g l fn = testCase l $ runReaderT (void fn) (Env g)
