{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GDAX.Test.Private
    ( tests
    ) where

import           Control.Lens
import           Network.GDAX.Explicit
import           Network.GDAX.Test.Types
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: Env -> TestTree
tests e = testGroup "Private"
    [ case_viewAccounts e
    ]

case_viewAccounts :: Env -> TestTree
case_viewAccounts e = testCaseSteps "ViewAccount" $ \step -> do
    step "listAccounts"
    accounts <- listAccounts (e ^. sandbox)

    step "getAccounts (indavidually for each)"
    _singles <- mapM (getAccount (e ^. sandbox) . _accountId) accounts

    return ()
