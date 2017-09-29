{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GDAX.Test.Private
    ( tests
    ) where

import           Control.Lens
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as LBS
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
    methods <- listPaymentMethods (e ^. sandbox)
    return ()

case_listCoinbaseAccounts :: Env -> TestTree
case_listCoinbaseAccounts e = testCase "list_coinbase_accounts" $ do
    cbaseAccounts <- listCoinbaseAccounts (e ^. sandbox)
    return ()

case_useCoinbaseAccount :: Env -> TestTree
case_useCoinbaseAccount e = testCaseSteps "use_coinbase_account" $ \step -> do
    accounts <- listCoinbaseAccounts (e ^. sandbox)
    return ()
    -- print accounts
