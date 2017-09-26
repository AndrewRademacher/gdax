{-# LANGUAGE TemplateHaskell #-}

module Network.GDAX.Test.Types where

import           Control.Lens
import           Network.GDAX.Implicit

data Env
    = Env
        { _envGdax :: Gdax
        }

$(makeClassy ''Env)

instance HasGdax Env where gdax = envGdax
