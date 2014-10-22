-- | This module re-export "Text.Parsec" (<http://hackage.haskell.org/package/parsec>)
--   and provides common parser tokens.
module Tct.Core.Common.Parser
  (
  module Text.Parsec
  -- * Basic Parsers
  , symbol
  , int
  , nat
  , bool

  -- * Parenthesis
  , braces
  , parens

  -- * Lexeme and Special Parsers
  , lexeme
  , reserved
  , identifier
  , whiteSpace
  ) where


import           Text.Parsec
import qualified Text.Parsec.Token             as PT
import           Text.ParserCombinators.Parsec (CharParser)

import           Tct.Core.Data.Types           (strategyTP)


lexeme :: CharParser s a -> CharParser s a
lexeme = PT.lexeme strategyTP

symbol :: String -> CharParser s String
symbol = PT.symbol strategyTP

reserved :: String -> CharParser s ()
reserved = PT.reservedOp strategyTP

braces :: CharParser s a -> CharParser s a
braces = PT.braces strategyTP

parens :: CharParser s a -> CharParser s a
parens = PT.parens strategyTP

nat :: CharParser s Int
nat = fromInteger `fmap` PT.natural strategyTP

bool :: CharParser s Bool
bool = try (symbol "True" >> return True) <|> (symbol "False" >> return False)

int :: CharParser s Integer
int = PT.natural strategyTP

identifier :: CharParser s String
identifier = PT.identifier strategyTP

whiteSpace :: CharParser s ()
whiteSpace = PT.whiteSpace strategyTP

