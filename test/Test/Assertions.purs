module Test.Assertions
  ( assertions
  ) where

import Prelude
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (print)
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Array (zip)
import Data.List (List(..), singleton, toList)
import Data.Tuple (Tuple(..))
import Data.Semiring.Free (runFree)

import Text.SlamSearch
import Text.SlamSearch.Printer
import Text.SlamSearch.Types

assert x =
  if not x
  then throwException $ error $ "error in assertion"
  else pure unit

inputs =
  [ ">2"
  , "foo"
  , "+foo"
  , "-foo"
  , "#foo"
  , "*"
  , "uni*"
  , "foo:>2"
  , "foo:0..2"
  , "-foo:0..2"
  , "foo:bar:baz"
  , "baz:~\"_foo%bar\""
  , "~?foo*bar"
  , "foo:uni*"
  , "@path:/foo/bar"
  , "path:\"foo bar\""
  ]

expected = Term <$>
   [ {include: true, labels: Nil, predicate: Gt (Text "2")}
   , {include: true, labels: Nil, predicate: Contains (Text "foo")}
   , {include: true, labels: Nil, predicate: Contains (Text "foo")}
   , {include: false, labels: Nil, predicate: Contains (Text "foo")}
   , {include: true, labels: Nil, predicate: Contains (Tag "foo")}
   , {include: true, labels: Nil, predicate: Contains (Text "*")}
   , {include: true, labels: Nil, predicate: Contains (Text "uni*")}
   , {include: true, labels: singleton $ Common "foo",
      predicate: Gt (Text "2")}
   , {include: true, labels: singleton $ Common "foo",
      predicate: Contains (Range "0" "2")}
   , {include: false, labels: singleton $ Common "foo",
      predicate: Contains (Range "0" "2")}
   , {include: true, labels: toList [Common "foo", Common "bar"],
      predicate: Contains (Text "baz")}
   , {include: true, labels: singleton $ Common "baz",
      predicate: Like "_foo%bar"}
   , {include: true, labels: Nil,
      predicate: Like "?foo*bar"}
   , {include: true, labels: singleton $ Common "foo",
      predicate: Contains (Text "uni*")}
   , {include: true, labels: singleton $ Meta "path",
      predicate: Contains (Text "/foo/bar")}
   , {include: true, labels: singleton $ Common "path",
      predicate: Contains (Text "foo bar")}
   ]

assertions = do
  let actual = mkQuery <$> inputs
      tests = zip actual expected

  for_ tests \(Tuple a e) ->
    case a of
      Left _ -> assert false
      Right f ->
        case runFree f of
          Cons (Cons res Nil) Nil -> do
            print res
            print e
            assert $ res == e
          _ -> assert false
