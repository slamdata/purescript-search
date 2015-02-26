module Text.SlamSearch.Parser.Utils where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Data.Either
  
takeTok :: forall m a. (Monad m) => ParserT [a] m a
takeTok = ParserT $ \s ->
  return $ case s of
    x:xs -> {consumed: true,
             input: xs,
             result: Right x}
    _ -> {consumed: false,
          input: s,
          result: Left (ParseError {message: "there is nothing in any"})}

when :: forall m a. (Monad m) => (a -> Boolean) -> ParserT [a] m a
when f = try $ do
  a <- takeTok
  if f a then return a
    else fail "token doesn't satisfy when test"

get :: forall a m. (Monad m, Eq a) => a -> ParserT [a] m a
get token = when ((==) token)
