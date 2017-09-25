module Network.GDAX.Parsers where

import           Data.Aeson.Types
import           Data.Scientific
import qualified Data.Text           as T
import qualified Data.Vector.Generic as V
import           Text.Read

textScientific :: Value -> Parser Scientific
textScientific = withText "Text Scientific" $ \t ->
    case readMaybe (T.unpack t) of
        Just n  -> pure n
        Nothing -> fail "Could not parse string scientific."

textDouble :: Value -> Parser Double
textDouble = withText "Text Double" $ \t ->
    case readMaybe (T.unpack t) of
        Just n  -> pure n
        Nothing -> fail "Could not parse string double."

newtype StringDouble = StringDouble { unStringDouble :: Double }

instance FromJSON StringDouble where
    parseJSON = withText "Text Double" $ \t ->
        case readMaybe (T.unpack t) of
            Just n  -> pure (StringDouble n)
            Nothing -> fail "Could not parse string double."

bookItem :: (FromJSON c) => String -> (Double -> Double -> c -> d) -> Value -> Parser d
bookItem name f = withArray name $ \a -> f
        <$> (unStringDouble <$> parseJSON (a V.! 0))
        <*> (unStringDouble <$> parseJSON (a V.! 1))
        <*> parseJSON (a V.! 2)
