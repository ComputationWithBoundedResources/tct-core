{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tct.Core.Processor 
  ( 
    Result (..)
  , Processor (..)
  , SomeProcessor (..)

  , Fork
  , ProofData
  , CertificateFn

  , ErroneousProcessor (..)
  , ErroneousProof (..)
  ) where


import           Data.Foldable as F
import           Data.Traversable as T
import           Data.Typeable
import           Text.ParserCombinators.Parsec (CharParser, choice, string)
import           Text.ParserCombinators.Parsec.Prim (getState, try) 
import           Text.Parsec.Prim (parserFail)

import           Tct.Core.TctM
import           Tct.Core.Forks (Id)
import qualified Tct.Core.Certificate as C
import qualified Tct.Options as O
import qualified Tct.Pretty as PP
import qualified Tct.Xml as Xml


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

class (Typeable p, Show p, ProofData (ProofObject p), ProofData (Problem p), Fork (Forking p)) => Processor p where
  type ProofObject p :: *
  type Problem p     :: *
  type Forking p     :: * -> *
  type Forking p     = Id
  name               :: p -> String
  description        :: p -> String
  description        = name
  options            :: p -> O.Options [ SomeProcessor (Problem p) ] p
  options            = const []
  solve              :: p -> Problem p -> TctM (Result p)



class (Processor p, Typeable p) => ParsableProcessor p prob where
  parseProcessor :: p -> ProcessParser p prob

instance (Processor p, Problem p ~ prob) => ParsableProcessor p prob where
  parseProcessor p = string (name p) >> return p

type ProcessParser p prob = CharParser [SomeProcessor prob] p 

data SomeProcessor prob where
  SomeProc :: (ParsableProcessor p prob, Problem p ~ prob) => p -> SomeProcessor prob
  deriving Typeable
 
parseSomeProcessor :: Processor p => SomeProcessor prob -> ProcessParser p (Problem p)
parseSomeProcessor (SomeProc p) = maybe (parserFail "parseSomeProcessor") parseProcessor $ cast p

parseAnyProcessor :: Processor p => [SomeProcessor prob] -> ProcessParser p (Problem p)
parseAnyProcessor ps = choice [ try $ parseSomeProcessor p | p <- ps]
  
  --
--
  --

-- what to do;
-- save type representation in someproc; so that cast is more or less safe fore parsing; its either any way



{-parseSomeProcessor :: (ParsableProcessor p, Processor p, Problem p ~ prob) => SomeProcessor prob -> ProcessParser p prob-}
{-parseSomeProcessor (SomeProc proc) = parseProcessor proc-}

{-parseAnyProcessor :: ProcessParser p prob-}
{-parseAnyProcessor = getState >>= parseSomeProcessors-}

{-parseSomeProcessors :: [SomeProcessor prob] -> ProcessParser p prob-}
{-parseSomeProcessors = undefined-}

--data SomeProofObject = SomeProofObject deriving Show
--instance PP.Pretty SomeProofObject where pretty = PP.text . show
--data SomeForking a = SomeForking deriving (Show, Functor, Foldable , Traversable)
--data SomeProblem = SomeProblem deriving Show
--instance PP.Pretty SomeProblem where pretty = PP.text . show

--instance ProofData prob => Processor (SomeProcessor prob) where
  --type ProofObject (SomeProcessor prob) = SomeProofObject
  --type Problem (SomeProcessor prob)     = prob
  --type Forking (SomeProcessor prob)     = SomeForking
  --name (SomeProc p)                     = name p
  --description (SomeProc p)              = description p
  --solve (SomeProc _)                    = error "Tct.Core.Processor.solve: should not be called"

-- should not be; as we modify the instance with options

-- processor knows how to parse itself
--class (Processor p, Problem p ~ prob)  => ParsableProcessor p prob where
  --parseProcessor :: p -> ProcessParser p

--instance (Processor p, Problem p ~ prob) => ParsableProcessor p prob where
  --parseProcessor = undefined


--instance ProofData prob => ParsableProcessor (SomeProcessor prob) prob where
  --parseProcessor (SomeProc p) = SomeProc `liftM` parseProcessor p

{-instance (ProofData q,Processor p) => ParsableProcessor (SomeProcessor q) q where-}
  {-parseProcessor = undefined-}

{-instance ProofData q => Processor (SomeProcessor q) where-}


{-instance Show (SomeProcessor prob) where show (SomeProc p) = show p-}

-- Error Processor ----------------------------------------------------------- 
data ErroneousProof p = ErroneousProof IOError p deriving Show

instance Processor p => Xml.Xml (ErroneousProof p) where
  toXml (ErroneousProof err p) = 
    Xml.elt "error" [] [ Xml.elt "processor" [] [Xml.text (name p)]
                       , Xml.elt "message" [] [Xml.text (show err)] ]

instance Processor p => PP.Pretty (ErroneousProof p) where 
  pretty (ErroneousProof err p) = 
    PP.text "Processor" PP.<+> PP.squotes (PP.text (name p)) PP.<+> PP.text "signalled the following error:"
    PP.<$$> PP.indent 2 (PP.paragraph (show err))

data ErroneousProcessor p = ErroneousProc IOError p deriving (Show, Typeable)

instance Processor p => Processor (ErroneousProcessor p) where
    type ProofObject (ErroneousProcessor p) = ErroneousProof p
    type Problem (ErroneousProcessor p)     = Problem p
    name (ErroneousProc err p)              = name p ++ "[error: " ++ show err ++ "]"
    solve (ErroneousProc err p) _           = return (Fail (ErroneousProof err p))

