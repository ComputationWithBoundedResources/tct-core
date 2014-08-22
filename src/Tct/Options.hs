module Tct.Options
  (
    eopt
  , option
  , switch
  , flag
  , argument
  , withArgLong
  , withDefault
  , withHelpDoc
  , withMetavar
  , mkArgParser
  ) where


import           Data.Monoid                          (Monoid, (<>))
import qualified Options.Applicative                  as O
import qualified Options.Applicative.Builder.Internal as O

import           Tct.Pretty                           (Doc)

withArgLong :: O.HasName f => O.Mod f a -> String -> O.Mod f a
opt `withArgLong` str = opt <> O.long str

withDefault :: (O.HasValue f, Show a) => O.Mod f a -> a -> O.Mod f a
opt `withDefault` a = opt <> O.showDefault <> O.value a

withHelpDoc :: O.Mod f a -> Doc -> O.Mod f a
opt `withHelpDoc` doc = opt <> O.helpDoc (Just doc)

withMetavar :: O.HasMetavar f => O.Mod f a -> String -> O.Mod f a
opt `withMetavar` str = opt <> O.metavar str

eopt :: Monoid m => m
eopt = O.idm

option :: Read a => O.Mod O.OptionFields a -> O.Parser a
option = O.option

flag :: a -> a -> O.Mod O.FlagFields a -> O.Parser a
flag = O.flag

switch :: O.Mod O.FlagFields Bool -> O.Parser Bool
switch = O.switch

argument :: (String -> Maybe a) -> O.Mod O.ArgumentFields a -> O.Parser a
argument = O.argument

mkArgParser :: Show a => O.Parser a -> Doc -> O.ParserInfo a
mkArgParser par doc = O.info par (O.progDescDoc $ Just doc)

