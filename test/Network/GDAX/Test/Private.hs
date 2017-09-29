{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GDAX.Test.Private
    ( tests
    ) where

import           Control.Lens
import qualified Data.HashMap.Strict     as Map
import qualified Data.Vector.Generic     as V
import           Network.GDAX.Explicit
import           Network.GDAX.Test.Types
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: Env -> TestTree
tests e = testGroup "private"
    [ case_viewAccounts e
    , case_listPaymentMethods e
    , case_listCoinbaseAccounts e
    , case_useCoinbaseAccount e
    ]

case_viewAccounts :: Env -> TestTree
case_viewAccounts e = testCaseSteps "view_account" $ \step -> do
    step "listAccounts"
    accounts <- listAccounts (e ^. sandbox)

    step "getAccounts (indavidually for each)"
    _singles <- mapM (getAccount (e ^. sandbox) . _accountId) accounts

    step "getAccountHistory (indavidually for each)"
    _histories <- mapM (getAccountHistory (e ^. sandbox) . _accountId) accounts

    return ()

case_listPaymentMethods :: Env -> TestTree
case_listPaymentMethods e = testCase "list_payment_methods" $ do
    _methods <- listPaymentMethods (e ^. sandbox)
    return ()

case_listCoinbaseAccounts :: Env -> TestTree
case_listCoinbaseAccounts e = testCase "list_coinbase_accounts" $ do
    _cbaseAccounts <- listCoinbaseAccounts (e ^. sandbox)
    return ()

case_useCoinbaseAccount :: Env -> TestTree
case_useCoinbaseAccount e = testCaseSteps "use_coinbase_account" $ \step -> do
    step "Get account list."
    accountMap <- getAccountMap
    let (Just usd) = Map.lookup "USD Wallet" accountMap
        (Just btc) = Map.lookup "Fake" accountMap

    step "Make USD deposit."
    _rDepositUSD <- depositCoinbase gdax (CoinbaseDeposit 10000 "USD" (_cbaccountId usd))

    step "Make BTC deposit."
    _rDepositBTC <- depositCoinbase gdax (CoinbaseDeposit 10 "BTC" (_cbaccountId btc))

    step "Make USD withdraw."
    _rWithdrawUSD <- withdrawCoinbase gdax (CoinbaseWithdraw 10000 "USD" (_cbaccountId usd))

    step "Make BTC withdraw."
    _rWithdrawBTC <- withdrawCoinbase gdax (CoinbaseWithdraw 10 "BTC" (_cbaccountId btc))

    return ()
    where
        gdax = e ^. sandbox
        getAccountMap = Map.fromList . V.toList . fmap (\a -> (_cbaccountName a, a)) <$> listCoinbaseAccounts gdax
