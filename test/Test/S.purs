module Test.S where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Test.Mocha

import Data.Semiring.Free
import Data.Tuple
import Data.Foldable
import Data.Either

import Text.SlamSearch
import Text.SlamSearch.Types

import Debug.Trace
import Debug.Foreign

assert x = do
  if not x then
    throwException $ error $ "error in assertion"
    else return unit

searchTest = do
  describe "mkQuery" $ do
    it "should parse queries" $ do
      let inputs = [
            ">2",
            "foo",
            "+foo",
            "-foo",
            "#foo",
            "*",
            "uni*",
            "foo:>2",
            "foo:0..2",
            "-foo:0..2",
            "foo:bar:baz",
            "baz:~\"_foo%bar\"",
            "~?foo*bar",
            "foo:uni*",
            "@path:/foo/bar",
            "path:\"foo bar\""
            ]
          expected = Term <$> [
            {include: true, labels: [], predicate: Gt (Text "2")},
            {include: true, labels: [], predicate: Contains (Text "foo")},
            {include: true, labels: [], predicate: Contains (Text "foo")},
            {include: false, labels: [], predicate: Contains (Text "foo")},
            {include: true, labels: [], predicate: Contains (Tag "foo")},
            {include: true, labels: [], predicate: Contains (Text "*")},
            {include: true, labels: [], predicate: Contains (Text "uni*")},
            {include: true, labels: [Common "foo"],
             predicate: Gt (Text "2")},
            {include: true, labels: [Common "foo"],
             predicate: Contains (Range "0" "2")},
            {include: false, labels: [Common "foo"],
             predicate: Contains (Range "0" "2")},
            {include: true, labels: [Common "foo", Common "bar"],
             predicate: Contains (Text "baz")},
            {include: true, labels: [Common "baz"],
             predicate: Like "_foo%bar"},
            {include: true, labels: [],
             predicate: Like "?foo*bar"},
            {include: true, labels: [Common "foo"],
             predicate: Contains (Text "uni*")},
            {include: true, labels: [Meta "path"],
             predicate: Contains (Text "/foo/bar")},
            {include: true, labels: [Common "path"],
             predicate: Contains (Text "foo bar")}
            ]
          actual = mkQuery <$> inputs
          tests = zip actual expected
      
      for_ tests (\(Tuple a e) -> 
                     case a of
                       Left _ -> assert false
                       Right f ->
                         case runFree f of
                           [[res]] -> do
                             print res
                             print e
                             assert $ res == e
                           _ -> assert false)

spec = do
  searchTest

