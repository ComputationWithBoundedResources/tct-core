-- | This module provides a wrapper for 
-- <http://hackage.haskell.org/package/optparse-applicative OptParse.Applicative> 
-- to construct custom argument parsers.
module Tct.Common.Options
  (
    -- * Example
    {-|
    The parser for 'Tct.Processor.Combinators.TimeoutProcessor' is defined as follows:

    >
    >TimeoutProc
    >  <$> optional (option $ eopt
    >      `withArgLong` "untilT"
    >      `withMetavar` "iSec"
    >      `withHelpDoc` PP.paragraph "Aborts the computation after 'iSec' from the startint time.")
    >  <*> optional (option $ eopt
    >      `withArgLong` "inT"
    >      `withMetavar` "iSec"
    >      `withHelpDoc` PP.paragraph "Aborts the computation after 'iSec' from starting the sub processor.")
    >  <*> argument  (parseSomeProcessorMaybe ps) (eopt
    >      `withMetavar` "proc"
    >      `withHelpDoc` PP.string "The applied subprocessor.")
    >

    Following strings are successfully parsed::
        "processor",
        "--inT 60 processor",
        "--untilT 320 --inT 60 processor".
    
    Option @Parser a@ can be lifted to @Parser (Maybe a)@ using
    'Control.Applicative.optional'.  Option @Parser (Maybe a)@ is optional during
    parsing. If an option @Parser a@ is constructed using 'withDefault' the option
    is optional during parsing. If an option @Parser (Maybe a)@ is constructed
    using @'withDefault' val@ then the option returns @'Just' val@ if no other
    value is provided.
    -}

    -- * Functions
    -- ** Builder
    eopt
  , withArgLong
  , withDefault
  , withHelpDoc
  , withMetavar
    -- ** Option Type
  , option
  , switch
  , flag
  , argument
    -- ** Finisher
  , mkArgParser
  ) where


import           Data.Monoid                          ((<>))
import qualified Options.Applicative                  as O
import qualified Options.Applicative.Builder.Internal as O

import           Tct.Common.Pretty                           (Doc)


-- | Identity Option. Is used to construct the option.
eopt :: Show a => O.Mod f a
eopt = O.idm <> O.showDefault

-- | Sets the id for the argument. The id is parsed as "--id".
withArgLong :: O.HasName f => O.Mod f a -> String -> O.Mod f a
opt `withArgLong` str = opt <> O.long str

-- | Sets a default value for an option.
withDefault :: (O.HasValue f, Show a) => O.Mod f a -> a -> O.Mod f a
opt `withDefault` a = opt <> O.value a

-- | Sets the description for an option.
withHelpDoc :: O.Mod f a -> Doc -> O.Mod f a
opt `withHelpDoc` doc = opt <> O.helpDoc (Just doc)

-- | Sets the meta variable. Default meta variable is 'ARG'.
withMetavar :: O.HasMetavar f => O.Mod f a -> String -> O.Mod f a
opt `withMetavar` str = opt <> O.metavar str

-- | Constructs and Option given the empty option together with its modifiers.
option :: Read a => O.Mod O.OptionFields a -> O.Parser a
option = O.option

-- | A Flag option..
flag :: a -> a -> O.Mod O.FlagFields a -> O.Parser a
flag = O.flag

-- | A Boolesche Option, which is 'False' per default.
switch :: O.Mod O.FlagFields Bool -> O.Parser Bool
switch = O.switch

-- | An argument opton without any id.
argument :: (String -> Maybe a) -> O.Mod O.ArgumentFields a -> O.Parser a
argument = O.argument

-- | Given a parser and a description this function constructs the actual parser.
mkArgParser :: Show a => O.Parser a -> Doc -> O.ParserInfo a
mkArgParser par doc = O.info par (O.fullDesc <> O.progDescDoc (Just doc))

