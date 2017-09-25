module Network.GDAX.Exceptions where

import           Control.Monad.Catch
import           Data.Text           (Text)

data MalformedGDAXResponse
    = MalformedGDAXResponse Text
    deriving (Show)

instance Exception MalformedGDAXResponse
