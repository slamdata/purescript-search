module Test.S where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Test.Mocha

import Debug.Trace
import Debug.Foreign

assert x = do
  if not x then
    throwException $ error $ "error in assertion"
    else return unit

searchTest = do
  fprint "not implemented"

spec = do
  searchTest

