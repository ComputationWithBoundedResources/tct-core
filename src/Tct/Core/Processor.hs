-- | This module provides the 'Processor' type.
-- 'Processor' instances define a transformation of a problem to a (possible empy) set of subproblems and how the
-- results from the subproblems are combined.
module Tct.Core.Processor
  (
  -- * Processor
  ProofData
  , CertificateFn
  , Result (..)
  , Processor (..)

  -- * Existential Type
  , SomeProcessor (..)

  -- * Parsable Procesor
  , ParsableProcessor (..)
  , unitParser
  , argsParser
  , mkDescription
  , parseSomeProcessor
  , parseSomeProcessorMaybe
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
import qualified Tct.Core.Certificate     as C
import           Tct.Core.Forks           (Fork, Id (..))
import           Tct.Core.TctM
--import qualified Tct.Xml              as Xml


-- Processor -----------------------------------------------------------------

-- | Provides the interface for proof construction.
-- All types which occur in the proof construction have to implement 'ProofData'.
type ProofData d = (PP.Pretty d, Show d)
--type ProofData d = (Xml.Xml d, PP.Pretty d, Show d)

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

-- | Defines the transformation on problems.
class (Show p, ProofData (ProofObject p), ProofData (Problem p), Fork (Forking p)) => Processor p where
  -- | Defines the type of the proof. Has to be an instance of 'ProofData'.
  type ProofObject p :: *
  -- | Defines the type of the considered problem. Has to be an instance of 'ProofData'.
  type Problem p     :: *
  -- | Defines the type of the container for the subproblems.
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
data SomeProcessor :: * -> * where
  SomeProc :: (Processor p, ParsableProcessor p) => p -> SomeProcessor (Problem p)

instance Show (SomeProcessor prob) where
  show (SomeProc p) = show p


-- Parsable Processor --------------------------------------------------------
type ArgumentParser p = O.ParserInfo (SomeProcessor (Problem p))
type Description      = PP.Doc

-- | Instances of 'ParsableProcessor' provide string parser.
class Processor p => ParsableProcessor p where
  -- | Defines the argument parser for the instance.
  -- The default implementation takes no arguments.
  args           :: p -> [SomeProcessor (Problem p)] -> ArgumentParser p
  -- | Defines the actual parser for the processor.
  -- The default implemenation uses a combination of 'name' and 'args' to generate a parser.
  -- The default iplementation returns the processor itself if 'args' is not specified.
  -- See "Options" for an example implementation of 'args'.
  parseProcessor :: p -> [SomeProcessor (Problem p)] -> String -> Either TctError (SomeProcessor (Problem p))
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
unitParser :: ParsableProcessor a => a -> Description -> ArgumentParser a
unitParser p desc = SomeProc `fmap` O.info (O.pure p) (O.progDescDoc (Just desc))

-- | Define custom processor (argument) parser.
argsParser :: ParsableProcessor a => O.Parser a -> Description -> ArgumentParser a
argsParser parser desc = SomeProc `fmap` mkArgParser parser desc

-- | Generates a descripton from the 'ParsableProcessor' instance of a list of processors.
mkDescription :: [SomeProcessor prob] -> PP.Doc
mkDescription ps = PP.vcat $ map (mkDescription' ps) ps where

mkDescription' :: [SomeProcessor prob] -> SomeProcessor prob  -> PP.Doc
mkDescription' ps (SomeProc p) =
  PP.string "--" PP.<+> PP.string (name p) PP.<+> PP.string (replicate (74 - length (name p)) '-')
  PP.<$$>
  PP.indent 2 (PP.empty
  PP.<$$> O.helpText (O.parserHelp (O.prefs O.idm) (O.infoParser parser))
  PP.<$$> (PP.empty `fromMaybe`  O.unChunk (O.infoProgDesc parser))
  PP.<$$> PP.empty)
  where
    parser = args p ps

-- | Provides a simple parser for processors.
parseSomeProcessor :: (ParsableProcessor (SomeProcessor prob), Problem (SomeProcessor prob) ~ prob)
  => [SomeProcessor prob] -> String -> Either TctError (SomeProcessor prob)
parseSomeProcessor rs s =  def `fromMaybe` L.find isRight (map (\r -> parseProcessor r rs s) rs)
  where def = Left $ TctParseError $ "readAnyProc: ("  ++ s ++ ")"

-- | Like 'parseSomeProcessor' but returns 'Maybe'.
parseSomeProcessorMaybe ::
  (ParsableProcessor (SomeProcessor t), Problem (SomeProcessor t) ~ t) =>
  [SomeProcessor t] -> String -> Maybe (SomeProcessor t)
parseSomeProcessorMaybe rs s = hush $ parseSomeProcessor rs s

