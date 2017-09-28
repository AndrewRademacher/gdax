{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.GDAX.Explicit.Private where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import           Network.GDAX.Core
import           Network.GDAX.Types.Private
import           Network.GDAX.Types.Shared

listAccounts :: (MonadIO m, MonadThrow m) => Gdax -> m (Vector Account)
listAccounts g = gdaxSignedGet g "/accounts" []

getAccount :: (MonadIO m, MonadThrow m) => Gdax -> AccountId -> m Account
getAccount g aid = gdaxSignedGet g ("/accounts/" <> show aid) []

getAccountHistory :: (MonadIO m, MonadThrow m) => Gdax -> AccountId -> m (Vector Entry)
getAccountHistory g aid = gdaxSignedGet g ("/accounts/" <> show aid <> "/ledger") []

getAccountHolds :: (MonadIO m, MonadThrow m) => Gdax -> AccountId -> m (Vector Hold)
getAccountHolds g aid = gdaxSignedGet g ("/accounts/" <> show aid <> "/holds") []

placeOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewOrder -> m NewOrderConfirmation
placeOrder g no = gdaxSignedPost g "/orders" [] no

placeLimitOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewLimitOrder -> m NewOrderConfirmation
placeLimitOrder g no = gdaxSignedPost g "/orders" [] no

placeMarketOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewMarketOrder -> m NewOrderConfirmation
placeMarketOrder g no = gdaxSignedPost g "/orders" [] no

placeStopOrder :: (MonadIO m, MonadThrow m) => Gdax -> NewStopOrder -> m NewOrderConfirmation
placeStopOrder g no = gdaxSignedPost g "/orders" [] no

cancelOrder :: (MonadIO m, MonadThrow m) => Gdax -> OrderId -> m ()
cancelOrder g oid = gdaxSignedDelete g ("/orders/" <> show oid) []

cancelAllOrders :: (MonadIO m, MonadThrow m) => Gdax -> ProductId -> m (Vector OrderId)
cancelAllOrders g pid = gdaxSignedDelete g ("/orders") [("product_id", T.pack (show pid))]

listOrders :: (MonadIO m, MonadThrow m) => Gdax -> Set ProductId -> Set OrderStatus -> m (Vector Order)
listOrders g pids oss = gdaxSignedGet g "/orders" params
    where
        params = fmap (\p -> ("product_id", T.pack (show p))) (Set.toList pids)
            <> fmap (\s -> ("status", T.pack (show s))) (Set.toList oss)

getOrder :: (MonadIO m, MonadThrow m) => Gdax -> OrderId -> m Order
getOrder g oid = gdaxSignedGet g ("/orders/" <> show oid) []

listFills :: (MonadIO m, MonadThrow m) => Gdax -> Set OrderId -> Set ProductId -> m (Vector Fill)
listFills g oids pids = gdaxSignedGet g "/fills" params
    where
        params = fmap (\p -> ("product_id", T.pack (show p))) (Set.toList pids)
            <> fmap (\o -> ("order_id", T.pack (show o))) (Set.toList oids)

listFundings :: (MonadIO m, MonadThrow m) => Gdax -> Set FundingStatus -> m (Vector Funding)
listFundings g fs = gdaxSignedGet g "/fundings" (fmap (\f -> ("status", T.pack (show f))) (Set.toList fs))

repayFunding :: (MonadIO m, MonadThrow m) => Gdax -> CurrencyId -> Double -> m ()
repayFunding g c a = gdaxSignedPost g "/funding/repay" params ()
    where
        params = [ ("currency", T.pack (show c))
                 , ("amount", T.pack (show a))
                 ]

createMarginTransfer :: (MonadIO m, MonadThrow m) => Gdax -> NewMarginTransfer -> m MarginTransfer
createMarginTransfer g nmt = gdaxSignedPost g "/profiles/margin-transfer" [] nmt

getPosition :: (MonadIO m, MonadThrow m) => Gdax -> m Position
getPosition g = gdaxSignedGet g "/position" []

type RepayOnly = Bool

closePosition :: (MonadIO m, MonadThrow m) => Gdax -> RepayOnly -> m ()
closePosition g r = gdaxSignedPost g "/position/close" [("repay_only", str)] ()
    where
        str = if r then "true" else "false"

deposit :: (MonadIO m, MonadThrow m) => Gdax -> Deposit -> m DepositReceipt
deposit g d = gdaxSignedPost g "/deposits/payment-method" [] d

depositCoinbase :: (MonadIO m, MonadThrow m) => Gdax -> CoinbaseDeposit -> m CoinbaseDepositReceipt
depositCoinbase g d = gdaxSignedPost g "/deposits/coinbase-account" [] d

withdraw :: (MonadIO m, MonadThrow m) => Gdax -> Withdraw -> m WithdrawReceipt
withdraw g w = gdaxSignedPost g "/withdraws/payment-method" [] w

withdrawCoinbase :: (MonadIO m, MonadThrow m) => Gdax -> CoinbaseWithdraw -> m CoinbaseWithdrawReceipt
withdrawCoinbase g w = gdaxSignedPost g "/withdraws/payment-method" [] w
