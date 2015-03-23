module Main where

import Control.Monad.Eff
import Debug.Trace
import Debug.Foreign
import Control.Apply
import Control.Alt
import Control.Alternative
import Data.Foldable 
import Data.Semiring.Free
import Data.String
import Data.Either

import Text.SlamSearch

test :: String -> Eff _ Unit
test  = void <<< fprint <<< mkQuery

main = do
  traverse_ test  [
    "foo bar", "foo+", "-foo", "#foo", "*", "uni*", "foo:21",
    "bar:baz:quux", "-baz:12..12212", "@l:~foo*?12",
    ">12"]

  print "================"
  traverse_ test [
    ":::", ":+:foo foo"
    ]
