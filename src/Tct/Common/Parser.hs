module Tct.Common.Parser
  (
  tokenize
  ) where

import Data.Char (isSpace)

import Tct.Common.Error (TctError (..))

tokenize :: String -> Either TctError (String, [String])
tokenize cs = do
    ts <- filter (not .null) `fmap` token (whiteSpace cs) ""
    if null ts
      then Left $ TctParseError "tokenize: no token."
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
token' w _      _   _      = Left $ TctParseError $ "tokenize.token': illformed expression" ++ w

whiteSpace :: String -> String
whiteSpace = dropWhile isSpace

