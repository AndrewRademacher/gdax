module Network.GDAX.Parsers where

import           Data.Aeson.Types
import           Data.Scientific
import qualified Data.Text        as T
import           Text.Read

textScientific :: Value -> Parser Scientific
textScientific = withText "Text Scientific" $ \t ->
    case readMaybe (T.unpack t) of
        Just n  -> pure n
        Nothing -> fail "Could not parse string scientific."
