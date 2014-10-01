-- | This module provides common parsing functionalities.
module Tct.Common.Parser
  (
  tokenise

  , module P 
  , CharParser
  , lexer
  , whiteSpace
  , symbol
  , integer
  , natural
  ) where


import Control.Monad (void)
import Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language as P (haskellDef)

import Data.Char (isSpace)

import Tct.Common.Error (TctError (..))

type CharParser s = P.Parsec String s

lexer :: P.TokenParser st
lexer = P.makeTokenParser P.haskellDef

whiteSpace :: CharParser s ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> CharParser s ()
symbol s = void (P.symbol lexer s)

integer :: CharParser s Int
integer = fromIntegral `fmap` P.integer lexer

natural :: CharParser s Int
natural = fromIntegral `fmap` P.natural lexer

-- | @'tokenise' string@ splits 'string' in tokens. A token is defined as a word or a parenthesised expression.
-- Returns a 'TctError' if the string is empty or the parentheses are mismatched, otherwise (first token, rest token).
--
-- > tokenise "TimeoutProcessor --int 10 (OrderProcessor --bound linear)" = ["TimeoutProcessor", "--inT", "10", "OrderProcessor --bound linear"]
tokenise :: String -> Either TctError (String, [String])
tokenise cs = do
  ts <- filter (not .null) `fmap` tokenx (whiteSpace' cs) ""
  if null ts
    then Left $ TctParseError "Parser.tokenise: empty string."
    else return (head ts, tail ts)

tokenx :: String -> String -> Either TctError [String]
tokenx [] r       = return [r]
tokenx (' ':cs) r = tokenx (whiteSpace' cs) "" >>= \rs -> return (r :rs)
tokenx ('(':cs) r = tokenx' cs [] cs "" >>= \(r',cs') -> tokenx cs' "" >>= \rs -> return (r:r':rs)
tokenx (c:cs) r   = tokenx cs (r++[c])

tokenx' :: String -> [()] -> String -> String -> Either TctError (String, String)
tokenx' _ []     (')':cs) r = return (r,cs)
tokenx' w ns     ('(':cs) r = tokenx' w (():ns) cs r >>= \(r',cs') -> return ('(':r',cs')
tokenx' w (_:ns) (')':cs) r = tokenx' w ns cs r >>= \(r',cs') -> return (')':r',cs')
tokenx' w ns     (c:cs)   r = tokenx' w ns cs r >>= \(r',cs') -> return (c:r',cs')
tokenx' w _      _   _      = Left $ TctParseError $ "Parser.tokenx': mismatched parenthesis: " ++ w

whiteSpace' :: String -> String
whiteSpace' = dropWhile isSpace

