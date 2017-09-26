{-# LANGUAGE TemplateHaskell #-}

module Network.GDAX.Test.Types where

import           Control.Lens
import           Network.GDAX.Implicit

class HasSandbox a where
    sandbox :: Lens' a Gdax

class HasLiveUnsigned a where
    liveUnsigned :: Lens' a Gdax

data Env
    = Env
        { _envSandboxGdax      :: Gdax
        , _envLiveUnsignedGdax :: Gdax
        }

$(makeClassy ''Env)

instance HasSandbox Env where sandbox = envSandboxGdax
instance HasLiveUnsigned Env where liveUnsigned = envLiveUnsignedGdax
