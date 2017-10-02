module Network.GDAX.Feed
    ( module Core
    , module Exceptions
    , module Feed
    , module Shared

    , defaultClient
    ) where

import           Control.Monad.Catch
import qualified Data.Aeson                as Aeson
import qualified Data.Text                 as T
import           Network.GDAX.Core         as Core
import           Network.GDAX.Exceptions   as Exceptions
import           Network.GDAX.Types.Feed   as Feed
import           Network.GDAX.Types.Shared as Shared
import           Network.WebSockets

defaultClient :: Subscriptions -> (GdaxMessage -> IO b) -> Connection -> IO b
defaultClient subs handler conn = do
    sendTextData conn (Aeson.encode (Subscribe subs))
    loop
    where
        loop = do
            res <- receiveData conn
            let asSum = Aeson.eitherDecode res :: Either String GdaxMessage
            case asSum of
                Left er -> throwM $ MalformedGdaxResponse $ T.pack er
                Right v -> handler v >> loop
