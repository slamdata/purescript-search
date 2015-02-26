module Test.S where

import Control.Monad.Eff
import Test.Mocha

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Tokens
import Text.SlamSearch.Parser.Values


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


foreign import raise """
function raise() {
  throw new Error();
}
""" :: forall e. Eff e Unit

assert x = do
  if not x then
    raise
    else return unit

rawTextTest = do
  describe "correct raw text" $ do
    it "should parse 'abcdefфыва1234'" $ do
      assert $ case runParser "abcdefфыва1234" rawString of
        Right "abcdefфыва1234" -> true
        _ -> false
    it "should not parse '@34'" $ do
      assert $ case runParser "@34" rawString of
        Right x -> false
        Left y -> true 


slashedTest = do
  describe "slashed parser" $ do
    it "should parse '\t' as '\t'" $ do
      assert $ case runParser "\\t" slashed of
        Right "\\t" -> true
        _ -> false
    it "should not parse 't'" $ do
      assert $ case runParser "t" slashed of
        Left _ -> true
        Right _ -> false
    it "should parse '\"'" $ do
      assert $ case runParser "\\\"" slashed of
        Left _ -> false
        Right _ -> true


quotedTest = do
  describe "quotedString parser" $ do
    it "should be able to parse \"foo\"" $ do
      let res = case runParser "\"foo\"" quotedString of
            Right "\"foo\"" -> true
            _ -> false
      assert res
    it "should be able to parse \"foo\\\"\"" $ do
      assert $ case runParser "\"foo\\\"\"" quotedString of
        Right "\"foo\\\"\"" -> true
        _ -> false

    it "should not be able to parse string ended with '\'" $ do
      assert $ case runParser "foo\\" quotedString of
        Left _ -> true
        Right _ -> false

    it "should not be able to parse unquoted '\"'" $ do
      assert $ case runParser "foo\"bar" quotedString of
        Left _ -> true
        Right _ -> false

tokenTest = do
  describe "token" $ do
    it "should to do something" $ do
      let input = "foo = bar~:: *** \"foo\\\"bar\"" 
      let res :: Either ParseError [Token]
          res = runParser input tokenize 
      let expected = [
            Text("foo"),Eq,Text("bar"),Tilde,Colon,Colon,
            Star, Star, Star, Text("\"foo\\\"bar\"")]
      let toShow :: Either ParseError [Token]
          toShow = Right expected
      assert $ case res of
        Right tokens -> tokens == expected
        Left _ -> false
    it "should produce ne, lt, lte, gt, gte, and so on" $ do
      let input = "> <> >= != = < <= >="
          res = runParser input tokenize
          expected = [Gt, Ne, GtE, Ne, Eq, Lt, LtE, GtE]
      assert $ case res of
        Right tokens -> tokens == expected
        Left _ -> false


valueTest = do
  describe "vals parser" $ do
    it "should pass Eq, Ne through" $ do
      let input = [Eq, Ne]
      let expected = [Through Eq, Through Ne]
      let actual = runParser input vals
      assert $ case actual of
        Left _ -> false
        Right res -> res == expected
    it "should work with glob, range, etc" $ do
      let input = [Text "foo", Star, Text "bar",QMark, Text "1", 
                   Hash, Text "baz",
                   Text "baz", Colon,
                   At, Text "meta", Colon,
                   Text "quux",
                   Ne,
                   Text "foo", Range, Text "bar"]
      let expected = [Glob "foo*bar?1",
                      Tag "baz",
                      Label "baz",
                      MetaLabel "meta",
                      Value "quux",
                      Through Ne,
                      RangeVal "foo" "bar"]
      let actual = runParser input vals

      assert $ case actual of
        Left _ -> false
        Right res -> res == expected


searchTest = do
  describe "searchQuery" $ do
    it "should do something" $ do
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
            "foo bar baz:quux:0..2"
            ]

      let results = [
            [IncludeTerm (SimplePredicate (GtP(Value("2"))))],
            [IncludeTerm (SimplePredicate (ContainsP(Value("foo"))))],
            [IncludeTerm (SimplePredicate (ContainsP(Value("foo"))))],
            [ExcludeTerm (SimplePredicate (ContainsP(Value("foo"))))],
            [IncludeTerm (SimplePredicate (ContainsP(Tag("foo"))))],
            [IncludeTerm (SimplePredicate (ContainsP(Glob("*"))))],
            [IncludeTerm (SimplePredicate (ContainsP(Glob("uni*"))))],
            [IncludeTerm (InfieldPredicate [Common("foo")] (GtP(Value "2")))],
            [IncludeTerm (InfieldPredicate
                          [Common("foo")]
                          (ContainsP(RangeVal "0" "2")))],
            
            [ExcludeTerm (InfieldPredicate
                          [Common("foo")]
                          (ContainsP(RangeVal "0" "2")))],
            
            [IncludeTerm (InfieldPredicate 
                        [Common("foo"), Common("bar")]
                        (ContainsP(Value("baz"))))],
            
            [IncludeTerm (InfieldPredicate
                          [Common("baz")]
                          (LikeP(Value("\"_foo%bar\""))))],
            
            [IncludeTerm (SimplePredicate (LikeP(Glob("?foo*bar"))))],
            [IncludeTerm (InfieldPredicate
                          [Common("foo")]
                          (ContainsP(Glob("uni*"))))],
            
            [IncludeTerm (InfieldPredicate
                          [Meta("path")]
                          (ContainsP(Value("/foo/bar"))))],
            
            [IncludeTerm (SimplePredicate (ContainsP(Value("foo")))),
             IncludeTerm (SimplePredicate (ContainsP(Value("bar")))),
             IncludeTerm (InfieldPredicate 
                         [Common("baz"), Common("quux")]
                         (ContainsP(RangeVal "0" "2")))]
            ]
      let cases = zip inputs results
      for_ cases $ \(Tuple input expected) ->
                   case parseSearchQuery input of
                     Left msg -> assert false
                     Right actual -> do
                       assert $ actual == expected



spec = do
  rawTextTest
  slashedTest
  quotedTest
  tokenTest
  valueTest
  searchTest

