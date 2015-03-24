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

import Data.Semiring.Free 
import Text.SlamSearch
import Text.SlamSearch.Types 
import Text.SlamSearch.Printer 

test :: String -> Eff _ Unit
test  = void <<< fprint <<< mkQuery

term1 :: Term
term1 = {
  include: true,
  predicate: Eq (Text "foo"),
  labels: []
  }

term2 :: Term
term2 = {
  include: false,
  predicate: Lte (Range "100" "200"),
  labels: [Common "foo", Meta "bar"]
  }

main = do
  traverse_ test  [
    "foo bar", "foo+", "-foo", "#foo", "*", "uni*", "foo:21",
    "bar:baz:quux", "-baz:12..12212", "@l:~foo*?12",
    ">12"]

  print "================"
  traverse_ test [
    ":::", ":+:foo foo"
    ]

  print "++++++++++++++++"
  print $ printTerm term1
  print $ printTerm term2
  print $ printQuery (free term1 * free term2)
