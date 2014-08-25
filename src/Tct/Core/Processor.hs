module Tct.Core.Processor 
  ( 
    -- * Processor
    Result (..)
  , Processor (..)
  , Fork
  , ProofData
  , CertificateFn

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


import           Data.Either          (isRight)
import           Data.Foldable        as F
import           Data.List            as L( find)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (mempty)
import           Data.Traversable     as T
import qualified Options.Applicative  as O
import qualified Options.Applicative.Help  as O

import qualified Tct.Core.Certificate as C
import           Tct.Core.Forks       (Id( ..))
import           Tct.Core.TctM
import           Tct.Error            (TctError( ..), hush)
import           Tct.Parser           (tokenize)
import           Tct.Options
import qualified Tct.Pretty           as PP
--import qualified Tct.Xml              as Xml


-- Processor ----------------------------------------------------------------- 
type Fork t      = (Foldable t, Functor t, Traversable t)
type ProofData d = (PP.Pretty d, Show d)
--type ProofData d = (Xml.Xml d, PP.Pretty d, Show d)

type CertificateFn p = Forking p C.Certificate -> C.Certificate

data Result p
  = Fail { proofData :: ProofObject p }
  | Success 
    { subProblems   :: Forking p (Problem p) 
    , proofData     :: ProofObject p
    , certificateFn :: CertificateFn p }

class (Show p, ProofData (ProofObject p), ProofData (Problem p), Fork (Forking p)) => Processor p where
  type ProofObject p :: *
  type Problem p     :: *
  type Forking p     :: * -> *
  type Forking p     = Id
  name               :: p -> String
  description        :: p -> String
  description        = name
  solve              :: p -> Problem p -> TctM (Result p)


-- Parsable Processor -------------------------------------------------------- 
type ArgumentParser p = O.ParserInfo (SomeProcessor (Problem p))
type Description      = PP.Doc

class Processor p => ParsableProcessor p where
  args           :: p -> [SomeProcessor (Problem p)] -> ArgumentParser p
  parseProcessor :: p -> [SomeProcessor (Problem p)] -> String -> Either TctError (SomeProcessor (Problem p))
  args p _              = unitParser p (PP.paragraph $ description p)
  parseProcessor p ps ss = do
    (t,ts) <- tokenize ss
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
  
parseSomeProcessor :: (ParsableProcessor (SomeProcessor prob), Problem (SomeProcessor prob) ~ prob) 
  => [SomeProcessor prob] -> String -> Either TctError (SomeProcessor prob)
parseSomeProcessor rs s =  def `fromMaybe` L.find isRight (map (\r -> parseProcessor r rs s) rs)
  where def = Left $ TctParseError $ "readAnyProc: ("  ++ s ++ ")"

parseSomeProcessorMaybe :: 
  (ParsableProcessor (SomeProcessor t), Problem (SomeProcessor t) ~ t) => 
  [SomeProcessor t] -> String -> Maybe (SomeProcessor t)
parseSomeProcessorMaybe rs s = hush $ parseSomeProcessor rs s


data SomeProcessor :: * -> * where
  SomeProc :: (Processor p, ParsableProcessor p) => p -> SomeProcessor (Problem p)

instance Show (SomeProcessor prob) where
  show (SomeProc p) = show p


