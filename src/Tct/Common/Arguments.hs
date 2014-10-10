{-# LANGUAGE GADTs #-}
module Tct.Common.Arguments
  ( 
  Arguments
  , Option
  , option
  , mayoption
  , Options
  , mkParser
  , description

  , unitArguments
  , UnitArgument
  , unitArgument
  ) where


import Control.Applicative

import qualified Tct.Common.Pretty as PP
import qualified Tct.Common.Xml as Xml
import Tct.Common.Parser (CharParser)
import qualified Tct.Common.Parser as P


type Arguments s a = Options s a

-- | Abstract option type.
data Option s a = Option 
  String           -- ^ Keyword.
  (CharParser s a) -- ^ Meaning.
  [String]         -- ^ Help.
  Bool             -- ^ Is optional.

-- | Returns the keyword of the option.
keyword :: Option s a -> String
keyword (Option s _ _ _) = s

-- | Returns the meaning of the option.
meaning :: Option s a -> CharParser s a
meaning (Option _ m _ _) = m

-- | @'option' keyword meaning help@ constructs an 'Option'.
option :: String -> CharParser s a -> [String] -> Options s a
option s m xs = Opt $ Option s m xs False

-- | @'mayoption' keyword meaning help@ constructs an optional 'Option'.
mayoption :: String -> CharParser s a -> [String] -> Options s (Maybe a)
mayoption s m xs = optional . Opt $ Option s m xs True

-- | Returns the UnitArgument.
unitArguments :: Options s UnitArgument
unitArguments = Nil (Just UnitArgument)

instance Functor (Option s) where
  f `fmap` Option k m h v = Option k (f `fmap` m) h v

-- | Abstract type for a collection of 'Option'.
data Options s a where
  Nil :: Maybe a -> Options s a
  Opt :: Option s a -> Options s a
  Sum :: Options s a -> Options s a -> Options s a
  Mul :: Options s (a -> b) -> Options s a -> Options s b

instance Functor (Options s) where
  f `fmap` Nil ma    = Nil (f `fmap` ma)
  f `fmap` Opt opt   = Opt (f `fmap` opt)
  f `fmap` Sum o1 o2 = Sum (f `fmap` o1) (f `fmap` o2)
  f `fmap` Mul fo o  = Mul ((f.) `fmap` fo) o

instance Applicative (Options s) where
  pure                  = Nil . Just
  (Nil (Just fo)) <*> o = fo `fmap` o
  o1 <*> o2             = Mul o1 o2

instance Alternative (Options s) where
  empty                = Nil Nothing
  (Nil Nothing) <|> o2 = o2
  o1@(Nil _) <|> _     = o1
  o1 <|> o2            = Sum o1 o2


mkParser :: Options s a -> CharParser s a
mkParser (Nil (Just a)) = return a
mkParser o              = step o >>= maybe (fin $ close o) mkParser
  where
    fin (Nil (Just a)) = return a
    fin _              = P.unexpected "oh no"

-- evaluate default value
close :: Options s a -> Options s a
close (Sum o1 o2) = close o1 <|> close o2
close (Mul o1 o2) = close o1 <*> close o2
close (Opt _)     = Nil Nothing
close o           = o

-- returns Nothing if no --keyword has been successfully parsed
step :: Options s a -> CharParser s (Maybe (Options s a))
step (Opt opt) = do
  P.try (P.symbol ('-':'-':keyword opt))
  f <- meaning opt
  return (Just $ pure f)
  P.<|>
  return Nothing
step (Sum o1 o2) = step o1 <|> step o2
step (Mul o1 o2) = do
  mo1 <- step o1
  case mo1 of
    Just o1' -> return $ Just (o1' <*> o2)
    Nothing -> do
      mo2 <- step o2 
      return $ case mo2 of
        Just o2'-> Just (o1 <*> o2')
        Nothing -> Nothing
step _ = return Nothing

-- The rhs of 'Sum' is ignored as per construction it is used only together with optional.
-- | Collects descriptive information .
description :: Options s a -> [(String, [String], Bool)]
description (Nil _)                       = []
description (Sum o1 _)                    = description o1
description (Mul o1 o2)                   = description o1 ++ description o2
description (Opt (Option key _ help vol)) = [(key,help,vol)]


--- Arguments --------------------------------------------------------------------------------------------------------

data UnitArgument = UnitArgument

unitArgument :: UnitArgument
unitArgument = UnitArgument

instance Show UnitArgument where
  show UnitArgument = ""

instance PP.Pretty UnitArgument where
  pretty UnitArgument = PP.empty

instance Xml.Xml UnitArgument where
  toXml UnitArgument = undefined

{-data UnaryArg a = UnaryArg a-}

