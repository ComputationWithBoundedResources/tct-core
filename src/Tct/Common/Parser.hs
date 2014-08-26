-- | This module provides parsing functionalities.
module Tct.Common.Parser
  (
  tokenise
  ) where

import Data.Char (isSpace)

import Tct.Common.Error (TctError (..))


-- | Splits a string in tokens. A token is a word or a parenthesised expression.
--
-- > tokenise "TimeoutProcessor --int 10 (OrderProcessor --bound linear)" = ["TimeoutProcessor:, "--inT", "10", "OrderProcessor --bound linear"]
tokenise :: String -> Either TctError (String, [String])
tokenise cs = do
  ts <- filter (not .null) `fmap` token (whiteSpace cs) ""
  if null ts
    then Left $ TctParseError "tokenise: no token."
    else return (head ts, tail ts)

-- TODO: continuation or so
token :: String -> String -> Either TctError [String]
token [] r       = return [r]
token (' ':cs) r = token (whiteSpace cs) "" >>= \rs -> return (r :rs)
token ('(':cs) r = token' cs [] cs "" >>= \(r',cs') -> token cs' "" >>= \rs -> return (r:r':rs)
token (c:cs) r   = token cs (r++[c])


token' :: String -> [()] -> String -> String -> Either TctError (String, String)
token' _ []     (')':cs) r = return (r,cs)
token' w ns     ('(':cs) r = token' w (():ns) cs r >>= \(r',cs') -> return ('(':r',cs')
token' w (_:ns) (')':cs) r = token' w ns cs r >>= \(r',cs') -> return (')':r',cs')
token' w ns     (c:cs)   r = token' w ns cs r >>= \(r',cs') -> return (c:r',cs')
token' w _      _   _      = Left $ TctParseError $ "tokenise.token': illformed expression" ++ w

whiteSpace :: String -> String
whiteSpace = dropWhile isSpace
