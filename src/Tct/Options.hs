module Tct.Options
  ( 
    anyArg
  , boolArg
  , flagArg
  , arguArg
  ) where


import Data.Monoid
import Options.Applicative

import Tct.Pretty (Doc)

type Flag = String
type Meta = String
type Help = Doc

anyArg :: (Show a, Read a) => Flag -> Meta -> Help -> a -> Parser a
anyArg flg met doc def = option $ mconcat [long flg, metavar met, helpDoc (Just doc), value def, showDefault]

boolArg :: Flag -> Help -> Parser Bool
boolArg flg doc = switch $ mconcat [long flg, helpDoc (Just doc), showDefault]

flagArg :: Show a => Flag -> Help -> a -> a -> Parser a
flagArg flg doc def alt = flag def alt $ mconcat [long flg, helpDoc (Just doc), showDefault]

arguArg :: (String -> Maybe a) -> Meta -> Help -> Parser a
arguArg red met doc = argument red $ mconcat [metavar met, helpDoc (Just doc)]

{-lexer :: P.TokenParser st-}
{-lexer = P.makeTokenParser haskellDef-}

{-type Meaning s a = a -> CharParser s a-}

{-data Option s a = Option -}
  {-{ keyword  :: String-}
  {-, optional :: Bool-}
  {-, meaning  :: Meaning s a-}
  {-, help     :: [String] }-}

{-option :: Option s a -}
{-option = Option -}
  {-{ keyword  = undefined-}
  {-, optional = True-}
  {-, meaning  = undefined-}
  {-, help     = [] }-}

{-type Options s a = [Option s a]-}

{-data Arg s a = Arg (Maybe String) (CharParser s a)-}

{-(<#>) :: (b -> a -> b) -> Arg s a -> Meaning s b-}
{-(<#>) f (Arg expected p) b = do -}
  {-a <- maybe p (\ s -> p <?> s) expected-}
  {-return (f b a)-}
  
{-none :: Arg s ()-}
{-none = Arg Nothing $ return ()-}

{-nat :: Arg s Int-}
{-nat = Arg (Just "<nat>") $ fromIntegral `liftM` P.natural lexer-}

