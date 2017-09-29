{-# LANGUAGE OverloadedStrings #-}

module Network.GDAX.Parsers where

import           Data.Aeson.Types
import           Data.Monoid
import           Data.Scientific
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector.Generic as V
import           Text.Read

textScientific :: Value -> Parser Scientific
textScientific = withText "Text Scientific" $ \t ->
    case readMaybe (T.unpack t) of
        Just n  -> pure n
        Nothing -> fail "Could not parse string scientific."

textMaybeDouble :: Maybe Value -> Parser (Maybe Double)
textMaybeDouble Nothing  = return Nothing
textMaybeDouble (Just v) = Just <$> textRead v

textRead :: (Read a) => Value -> Parser a
textRead = withText "Text Read" $ \t ->
    case readMaybe (T.unpack t) of
        Just n  -> pure n
        Nothing -> fail "Could not read value from string."

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

withObjectOfType :: String -> Text -> (Object -> Parser a) -> Value -> Parser a
withObjectOfType name type' fn = withObject name $ \o -> do
    t <- o .: "type"
    if t == type'
        then fn o
        else fail $ T.unpack $ "Expected type 'subscribe' got '" <> t <> "'."

nothingToEmptyVector :: Maybe (Vector a) -> Vector a
nothingToEmptyVector (Just v) = v
nothingToEmptyVector Nothing  = V.empty
