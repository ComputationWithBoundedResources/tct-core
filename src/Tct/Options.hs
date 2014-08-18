module Tct.Options 
  ( 
    Option (..)
  , Options
  , option
  , none
  , nat
  , (<#>)
  , Meaning
  , Arg (..)
  ) where


import           Text.ParserCombinators.Parsec hiding (optional, option)
import qualified Text.ParserCombinators.Parsec.Token as P
import           Text.ParserCombinators.Parsec.Language (haskellDef)
import           Control.Monad (liftM)

lexer :: P.TokenParser st
lexer = P.makeTokenParser haskellDef

type Meaning s a = a -> CharParser s a

data Option s a = Option 
  { keyword  :: String
  , optional :: Bool
  , meaning  :: Meaning s a
  , help     :: [String] }

option :: Option s a 
option = Option 
  { keyword  = undefined
  , optional = True
  , meaning  = undefined
  , help     = [] }

type Options s a = [Option s a]

data Arg s a = Arg (Maybe String) (CharParser s a)

(<#>) :: (b -> a -> b) -> Arg s a -> Meaning s b
(<#>) f (Arg expected p) b = do 
  a <- maybe p (\ s -> p <?> s) expected
  return (f b a)
  
none :: Arg s ()
none = Arg Nothing $ return ()

nat :: Arg s Int
nat = Arg (Just "<nat>") $ fromIntegral `liftM` P.natural lexer

