module Test.Assertions
  ( assertions
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception as Exn

import Data.Array (zip)
import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.List (List(..), singleton, fromFoldable)
import Data.Semiring.Free (runFree)
import Data.Tuple (Tuple(..))

import Test.Effects (TEST_EFFECTS)
import Text.Parsing.Parser (ParseError)
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Types as SS

assert ∷ Boolean → Eff TEST_EFFECTS Unit
assert x =
  unless x $
    Exn.throwException $
      Exn.error $ "error in assertion"

inputs ∷ Array String
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

expected ∷ Array SS.Term
expected =
  SS.Term <$>
    [ { include: true
      , labels: Nil
      , predicate: SS.Gt (SS.Text "2")
      }
    , { include: true
      , labels: Nil
      , predicate: SS.Contains (SS.Text "foo")
      }
    , { include: true
      , labels: Nil
      , predicate: SS.Contains (SS.Text "foo")
      }
    , { include: false
      , labels: Nil
      , predicate: SS.Contains (SS.Text "foo")
      }
    , { include: true
      , labels: Nil
      , predicate: SS.Contains (SS.Tag "foo")
      }
    , { include: true
      , labels: Nil
      , predicate: SS.Contains (SS.Text "*")
      }
    , { include: true
      , labels: Nil
      , predicate: SS.Contains (SS.Text "uni*")
      }
    , { include: true
      , labels: singleton $ SS.Common "foo"
      , predicate: SS.Gt (SS.Text "2")
      }
    , { include: true
      , labels: singleton $ SS.Common "foo"
      , predicate: SS.Range (SS.Text "0") (SS.Text "2")
      }
    , { include: false
      , labels: singleton $ SS.Common "foo"
      , predicate: SS.Range (SS.Text "0") (SS.Text "2")
      }
    , { include: true
      , labels: fromFoldable [SS.Common "foo", SS.Common "bar"]
      , predicate: SS.Contains (SS.Text "baz")
      }
    , { include: true
      , labels: singleton $ SS.Common "baz"
      , predicate: SS.Like "_foo%bar"
      }
    , { include: true
      , labels: Nil
      , predicate: SS.Like "?foo*bar"
      }
    , { include: true
      , labels: singleton $ SS.Common "foo"
      , predicate: SS.Contains (SS.Text "uni*")
      }
    , { include: true
      , labels: singleton $ SS.Meta "path"
      , predicate: SS.Contains (SS.Text "/foo/bar")
      }
    , { include: true
      , labels: singleton $ SS.Common "path"
      , predicate: SS.Contains (SS.Text "foo bar")
      }
    ]

assertions ∷ Eff TEST_EFFECTS Unit
assertions = traverse_ traverseFn $ zip actual expected
  where
  actual ∷ Array (Either ParseError SS.SearchQuery)
  actual = map mkQuery inputs

  traverseFn ∷ Tuple (Either ParseError SS.SearchQuery) SS.Term → Eff TEST_EFFECTS Unit
  traverseFn (Tuple a e) = void do
    f ← either (const $ Exn.throwException $ Exn.error "incorrect query") pure a
    case runFree f of
      Cons (Cons res Nil) Nil → do
        logShow res
        logShow e
        assert $ res == e
      _ → assert false
