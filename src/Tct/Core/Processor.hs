-- | This module provides the 'Processor' type.
-- 'Processor' instances define transformations from problems to a (possible empty) set of subproblems.
module Tct.Core.Processor
  (
  -- * Processor
  ProofData
  , CertificateFn
  , Result (..)
  , Processor (..)

  -- * Parsable Procesor
  , ParsableProcessor (..)
  , SomeParsableProcessor (..)
  , ArgumentParser
  , unitParser
  , argsParser
  , mkDescription
  , parseSomeParsableProcessor
  , parseSomeParsableProcessorMaybe
  ) where


import           Data.Either              (isRight)
import           Data.List                as L (find)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (mempty)
import qualified Options.Applicative      as O
import qualified Options.Applicative.Help as O

import           Tct.Common.Error         (TctError (..), hush)
import           Tct.Common.Options
import           Tct.Common.Parser        (tokenise)
import qualified Tct.Common.Pretty        as PP
import qualified Tct.Common.Xml           as Xml
import qualified Tct.Core.Certificate     as C
import           Tct.Core.Forks           (Fork, Id (..))
import           Tct.Core.TctM
--import qualified Tct.Xml              as Xml


-- Processor -----------------------------------------------------------------

-- | Provides the interface for the proof construction.
-- All types which occur in the proof construction have to implement 'ProofData'.
type ProofData d = (Xml.Xml d, PP.Pretty d, Show d)

-- | Type synonym for functions that defines how a 'C.Certificate' is computed from a collection of @'C.Certificate's@.
type CertificateFn p = Forking p C.Certificate -> C.Certificate

-- | The result of applying a @'Processor' p@ to a problem.
data Result p
  = Fail
    { proofData :: ProofObject p }
  | Success
    { subProblems   :: Forking p (Problem p)
    , proofData     :: ProofObject p
    , certificateFn :: CertificateFn p }

-- | The interface to 'Processor' instances.
class (Show p, ProofData (ProofObject p), ProofData (Problem p), Fork (Forking p)) => Processor p where
  -- | The type of the proof. Has to be an instance of 'ProofData'.
  type ProofObject p :: *
  -- | The type of the considered problem. Has to be an instance of 'ProofData'.
  type Problem p     :: *
  -- | The type of the subproblems collection.
  -- Has to be an instance of 'Fork'. Default type is 'Id'.
  type Forking p     :: * -> *
  type Forking p     = Id
  -- | The name of the processor. Should be unique.
  name               :: p -> String
  -- | The description of the processor.
  -- Default value is 'name'.
  description        :: p -> String
  description        = name
  -- | Defines the application of the 'Processor' instance to a problem.
  solve              :: p -> Problem p -> TctM (Result p)


-- Existential Type ----------------------------------------------------------

-- | Existential type of 'Processor'.
data SomeParsableProcessor :: * -> * where
  SomeParsableProc :: ParsableProcessor p => p -> SomeParsableProcessor (Problem p)

instance Show (SomeParsableProcessor prob) where
  show (SomeParsableProc p) = show p


-- Parsable Processor --------------------------------------------------------
-- | Type synonym for an argument parser.
type ArgumentParser p = O.ParserInfo (SomeParsableProcessor (Problem p))

-- | Instances of 'ParsableProcessor' provide a string parser.
class Processor p => ParsableProcessor p where
  -- | The argument parser for the instance.
  -- The default implementation takes no arguments.
  args           :: p -> [SomeParsableProcessor (Problem p)] -> ArgumentParser p
  -- | Defines the actual parser for the processor.
  -- The default implemenation uses a combination of 'name' and 'args' to generate a parser.
  -- The default iplementation returns the processor itself if 'args' is not specified.
  -- See "Options" for an example implementation of 'args'.
  parseProcessor :: p -> [SomeParsableProcessor (Problem p)] -> String -> Either TctError (SomeParsableProcessor (Problem p))
  args p _              = unitParser p (PP.paragraph $ description p)
  parseProcessor p ps ss = do
    (t,ts) <- tokenise ss
    if name p == t
      then case O.execParserPure (O.prefs mempty) (args p ps) ts of
        O.Success a   -> Right a
        O.Failure err -> Left $ TctParseError $ "optParser error (" ++ show err ++ "," ++ show ss ++ ")"
        _             -> Left $ TctParseError $ "optParser completion error (" ++ show ss ++ ")"
      else Left $ TctParseError "optParser"

-- | Default processor (argument) parser without arguments.
unitParser :: ParsableProcessor a => a -> PP.Doc -> ArgumentParser a
unitParser p desc = SomeParsableProc `fmap` O.info (O.pure p) (O.progDescDoc (Just desc))

-- | Define custom processor (argument) parser.
argsParser :: ParsableProcessor a => O.Parser a -> PP.Doc -> ArgumentParser a
argsParser parser desc = SomeParsableProc `fmap` mkArgParser parser desc

-- | Generates a descripton from the 'ParsableProcessor' instance of a list of processors.
mkDescription :: [SomeParsableProcessor prob] -> PP.Doc
mkDescription ps = PP.vcat $ map (mkDescription' ps) ps

mkDescription' :: [SomeParsableProcessor prob] -> SomeParsableProcessor prob  -> PP.Doc
mkDescription' ps (SomeParsableProc p) =
  PP.string "--" PP.<+> PP.string (name p) PP.<+> PP.string (replicate (74 - length (name p)) '-')
  PP.<$$>
  PP.indent 2 (PP.empty
  PP.<$$> O.helpText (O.parserHelp (O.prefs O.idm) (O.infoParser parser))
  PP.<$$> (PP.empty `fromMaybe`  O.unChunk (O.infoProgDesc parser))
  PP.<$$> PP.empty)
  where
    parser = args p ps

-- | Provides a simple parser for processors.
parseSomeParsableProcessor :: (ParsableProcessor (SomeParsableProcessor prob), Problem (SomeParsableProcessor prob) ~ prob)
  => [SomeParsableProcessor prob] -> String -> Either TctError (SomeParsableProcessor prob)
parseSomeParsableProcessor rs s =  def `fromMaybe` L.find isRight (map (\r -> parseProcessor r rs s) rs)
  where def = Left $ TctParseError $ "readAnyProc: ("  ++ s ++ ")"

-- | Like 'parseSomeParsableProcessor' but returns 'Nothing' if the parsing failed.
parseSomeParsableProcessorMaybe ::
  (ParsableProcessor (SomeParsableProcessor t), Problem (SomeParsableProcessor t) ~ t) =>
  [SomeParsableProcessor t] -> String -> Maybe (SomeParsableProcessor t)
parseSomeParsableProcessorMaybe rs s = hush $ parseSomeParsableProcessor rs s

