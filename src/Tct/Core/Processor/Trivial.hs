-- | This module provides some trivial processors.
module Tct.Core.Processor.Trivial
  ( 
  -- * Failed
  failingDeclaration
  , failing
  -- * Succeed
  , succeedingDeclaration
  , succeeding
  -- * Named
  , namedDeclaration
  , named

  ) where


import           Tct.Core.Common.SemiRing        as PP
import qualified Tct.Core.Common.Pretty          as PP
import           Tct.Core.Data
import           Tct.Core.Data.Declaration.Parse as P ()


data TrivialProof
  = Failed
  | Succeeded (Maybe String)
  deriving Show

instance PP.Pretty TrivialProof where
  pretty Failed               = PP.text "Fail."
  pretty (Succeeded (Just s)) = PP.text "Success (" PP.<> PP.text s PP.<> PP.text ")."
  pretty (Succeeded _)        = PP.text "Success."

{-instance Xml.Xml FailProof where-}
  {-toXml _ = Xml.elt "fail" []-}


-- Failing -----------------------------------------------------------------------------------------------------------

-- | A processor that always fails.
data FailProcessor prob = FailProc deriving Show

instance ProofData prob => Processor (FailProcessor prob) where
  type ProofObject (FailProcessor prob) = TrivialProof
  type Problem (FailProcessor prob)     = prob
  declaration p = declareProcessor "failing" ["Processor 'failing' always fails."] () (Proc p)
  solve p prob  = return $ resultToTree p prob (Fail Failed)

-- Default 'FailProcessor' instance.
failingProcessor :: ProofData prob => FailProcessor prob
failingProcessor = FailProc

-- | The failing Strategy declaration.
failingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
failingDeclaration = declaration failingProcessor


-- | The failing Strategy.
failing :: ProofData prob => Strategy prob
failing = Proc failingProcessor


-- Succeeding --------------------------------------------------------------------------------------------------------

-- | A processor that always succeeds. 
data SuccessProcessor prob = SuccessProc deriving Show

instance ProofData prob => Processor (SuccessProcessor prob) where
  type ProofObject (SuccessProcessor prob) = TrivialProof
  type Problem (SuccessProcessor prob)     = prob
  declaration p = declareProcessor "succeeding" ["Processor 'succeeding' always succeeds."] () (Proc p)
  solve p prob  = return $ resultToTree p prob (Success (Id prob) (Succeeded Nothing) bigAdd)

-- Default 'SuccessProcessor' instance.
succeedingProcessor :: ProofData prob => SuccessProcessor prob
succeedingProcessor = SuccessProc

-- | The succeeding strategy declaration.
succeedingDeclaration :: ProofData prob => Declaration('[] :-> Strategy prob)
succeedingDeclaration = declaration succeedingProcessor

-- | The succeeding strategy..
succeeding :: ProofData prob => Strategy prob
succeeding = Proc succeedingProcessor


-- Named -------------------------------------------------------------------------------------------------------------

-- | A processor that names a given strategy.
data NamedProcessor prob = NamedProc String (Strategy prob) deriving Show

instance ProofData prob => Processor (NamedProcessor prob) where
  type ProofObject (NamedProcessor prob) = TrivialProof
  type Problem (NamedProcessor prob) = prob
  type ProcessorArgs (NamedProcessor prob) = 
    '[ Argument 'Required String
     , Argument 'Required (Strategy prob)] 
  declaration _ = declareProcessor 
    "named" 
    ["Processor 'named' always succedds."] 
    ( string
      `withName` "name"
      `withHelp` ["The name of the sub-strategy."]
    , strat
    ) 
    $ \nm st -> Proc (NamedProc nm st)
  solve p@(NamedProc nm _) prob = 
    return $ resultToTree p prob (Success (Id prob) (Succeeded (Just nm)) bigAdd)

-- Default named processor.
namedProcessor :: ProofData prob => NamedProcessor prob
namedProcessor = NamedProc "" failing

-- | The named strategy declaration.
namedDeclaration :: ProofData prob => Declaration(
  '[ Argument 'Required String
   , Argument 'Required (Strategy prob)] 
  :-> Strategy prob)
namedDeclaration = declaration namedProcessor

-- | @'named' id st@ identifies the strategy @st@ with @id@.
named :: ProofData prob => String -> Strategy prob -> Strategy prob
named nm = Proc . NamedProc nm

