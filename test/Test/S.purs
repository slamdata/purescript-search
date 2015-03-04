module Test.S where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Test.Mocha

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Tokens
import Text.SlamSearch.Parser.Values
import Text.SlamSearch.Printer

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Data.Either
import Data.Foldable
import Data.Tuple
import Control.Apply
import Control.Alt
import Control.Alternative
import Debug.Trace


assert x = do
  if not x then
    throwException $ error $ "error in assertion"
    else return unit

searchTest = do
  describe "searchTerm" $ do
    it "should parse one term" $ do
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
            "@path:/foo/bar"
            ]

      let results = [
            IncludeTerm (SearchTermSimple [] (GtPredicate(TextVal("2")))),
            IncludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo")))),
            IncludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo")))),
            ExcludeTerm (SearchTermSimple [] (ContainsPredicate(TextVal("foo")))),
            IncludeTerm (SearchTermSimple [] (ContainsPredicate(Tag("foo")))),
            IncludeTerm (SearchTermSimple [] (ContainsPredicate(Glob("*")))),
            IncludeTerm (SearchTermSimple [] (ContainsPredicate(Glob("uni*")))),
            IncludeTerm (SearchTermSimple [Common("foo")] (GtPredicate(TextVal "2"))),
            IncludeTerm (SearchTermSimple
                         [Common("foo")]
                         (ContainsPredicate(RangeVal "0" "2"))),
            
            ExcludeTerm (SearchTermSimple
                         [Common("foo")]
                         (ContainsPredicate(RangeVal "0" "2"))),
            
            IncludeTerm (SearchTermSimple 
                        [Common("foo"), Common("bar")]
                        (ContainsPredicate(TextVal("baz")))),
            
            IncludeTerm (SearchTermSimple
                          [Common("baz")]
                          (LikePredicate(TextVal("\"_foo%bar\"")))),
            
            IncludeTerm (SearchTermSimple [] (LikePredicate(Glob("?foo*bar")))),
            IncludeTerm (SearchTermSimple
                         [Common("foo")]
                         (ContainsPredicate(Glob("uni*")))),
            
            IncludeTerm (SearchTermSimple
                          [Meta("path")]
                          (ContainsPredicate(TextVal("/foo/bar"))))
            ]
      let cases = zip inputs results
      for_ cases $ \(Tuple input expected) ->
                   case parseSearchTerm input of
                     Left msg -> assert false
                     Right actual -> do
                       assert $ actual == expected

  describe "parseSearchQuery" $ do
    it "should parse all query at once" $ do
      let input = "foo bar baz"
          expected = SearchAnd (IncludeTerm
                                (SearchTermSimple []
                                 (ContainsPredicate
                                  (TextVal "foo"))))
                     (SearchAnd (IncludeTerm
                                 (SearchTermSimple []
                                  (ContainsPredicate
                                   (TextVal "bar"))))
                      (SearchAnd (IncludeTerm
                                  (SearchTermSimple []
                                   (ContainsPredicate
                                    (TextVal "baz"))))
                       EmptyQuery))


      print $ prettyQuery expected

      case parseSearchQuery "baz:~\"_foo%bar\" -quux:foo:12..23" of
        Left msg -> do
          assert false
        Right actual -> do
          print $ prettyQuery actual
      
      case parseSearchQuery input of
        Left msg -> do
          assert false
        Right actual -> do
          print "!!!"
          assert $ actual == expected


spec = do
  searchTest

