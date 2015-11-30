module Test.Assertions
  ( assertions
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (print)
import Data.Foldable (traverse_)
import Data.Either (Either(), either)
import Data.Array (zip)
import Data.List (List(..), singleton, toList)
import Data.Tuple (Tuple(..))
import Data.Semiring.Free (runFree)

import Text.SlamSearch
import Text.SlamSearch.Types
import Text.Parsing.Parser (ParseError())

import Test.Effects (TEST_EFFECTS())

assert :: Boolean -> Eff TEST_EFFECTS Unit
assert x = when (not x) $ throwException $ error $ "error in assertion"

inputs :: Array String
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

expected :: Array Term
expected = Term <$>
   [ { include: true
     , labels: Nil
     , predicate: Gt (Text "2")
     }
   , { include: true
     , labels: Nil
     , predicate: Contains (Text "foo")
     }
   , { include: true
     , labels: Nil
     , predicate: Contains (Text "foo")
     }
   , { include: false
     , labels: Nil
     , predicate: Contains (Text "foo")
     }
   , { include: true
     , labels: Nil
     , predicate: Contains (Tag "foo")
     }
   , { include: true
     , labels: Nil
     , predicate: Contains (Text "*")
     }
   , { include: true
     , labels: Nil
     , predicate: Contains (Text "uni*")
     }
   , { include: true
     , labels: singleton $ Common "foo"
     , predicate: Gt (Text "2")
     }
   , { include: true
     , labels: singleton $ Common "foo"
     , predicate: Range (Text "0") (Text "2")
     }
   , { include: false
     , labels: singleton $ Common "foo"
     , predicate: Range (Text "0") (Text "2")
     }
   , { include: true
     , labels: toList [Common "foo", Common "bar"]
     , predicate: Contains (Text "baz")
     }
   , { include: true
     , labels: singleton $ Common "baz"
     , predicate: Like "_foo%bar"
     }
   , { include: true
     , labels: Nil
     , predicate: Like "?foo*bar"
     }
   , { include: true
     , labels: singleton $ Common "foo"
     , predicate: Contains (Text "uni*")
     }
   , { include: true
     , labels: singleton $ Meta "path"
     , predicate: Contains (Text "/foo/bar")
     }
   , { include: true
     , labels: singleton $ Common "path"
     , predicate: Contains (Text "foo bar")
     }
   ]

assertions :: Eff TEST_EFFECTS Unit
assertions = traverse_ traverseFn $ zip actual expected
  where
  actual :: Array (Either ParseError SearchQuery)
  actual = map mkQuery inputs

  traverseFn :: Tuple (Either ParseError SearchQuery) Term -> Eff TEST_EFFECTS Unit
  traverseFn (Tuple a e) = void do
    f <- either (const $ throwException $ error "incorrect query") pure a
    case runFree f of
      Cons (Cons res Nil) Nil -> do
        print res
        print e
        assert $ res == e
      _ -> assert false
