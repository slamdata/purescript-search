module Test.Effects where

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

type TEST_EFFECTS = (exception ∷ EXCEPTION, random ∷ RANDOM, console ∷ CONSOLE)
