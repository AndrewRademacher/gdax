module Network.GDAX.Exceptions where

import           Control.Monad.Catch
import           Data.Text           (Text)

data MalformedGdaxResponse
    = MalformedGdaxResponse Text
    deriving (Show)

instance Exception MalformedGdaxResponse
