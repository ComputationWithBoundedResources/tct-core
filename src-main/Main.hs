module Main where

import           Control.Applicative (optional)

import Tct.Trs.Trs
import Tct.Trs.Poly.NaturalPolynomialInterpretation

import Tct
import Tct.Core hiding (linear)
import Tct.Combinators
import Tct.Common.Options
import qualified Tct.Common.Pretty as PP
import qualified Tct.Common.Xml as Xml

main :: IO ()
main = applyMode trsMode

trsMode :: TctMode (TrsProblem Fun Var) CC
trsMode = TctMode
  { modeParser          = parser
  , modeStrategies      = 
      [ SomeParsableProc bestPolys'
      , SomeParsableProc fastestPolys' ]
  , modeDefaultStrategy = bestPolys
  , modeOptions         = options
  , modeModifyer        = modifyer
  , modeAnswer          = answering }


bestPolys :: Strategy (TrsProblem Fun Var)
bestPolys = es linear >=> es quadratic >=> es (mixed 2) >=> es (mixed 3)
  where es = exhaustively

fastestPolys :: Strategy (TrsProblem Fun Var)
fastestPolys = exhaustively $ linear <||> quadratic <||> mixed 2 <||> mixed 3

withTimeout :: Strategy (TrsProblem Fun Var) -> Maybe Int -> Strategy (TrsProblem Fun Var)
withTimeout st (Just i) = strat $ timeoutIn i st
withTimeout st Nothing  = st

bestPolys' :: CustomStrategy (Maybe Int) (TrsProblem Fun Var)
bestPolys' = withTimeout' bestPolys "bestpolys" doc
  where doc = "exhaustively applies polynomial interpretations upto degree 3 in order"

fastestPolys' :: CustomStrategy (Maybe Int) (TrsProblem Fun Var)
fastestPolys' = withTimeout' fastestPolys  "fastestpolys" doc
  where doc = "exhaustively applies polynomial interpretations upto degree 3 in parallel, returning whichever returns first"

withTimeout' :: Strategy (TrsProblem Fun Var) -> String -> String -> CustomStrategy (Maybe Int) (TrsProblem Fun Var)
withTimeout' st nm doc = strategy nm pargs (withTimeout st) Nothing
  where 
    cargs = optional . option $ eopt
      `withArgLong` "timeout" 
      `withHelpDoc` PP.paragraph "abort after nSec seconds" 
      `withMetavar` "nSec"
    pargs = mkArgParser cargs (PP.paragraph  doc)

{-instance (PP.Pretty a, PP.Pretty b) => PP.Pretty (Either a b) where-}
  {-pretty = undefined-}
{-instance (Xml.Xml a, Xml.Xml b) => Xml.Xml (Either a b) where-}
  {-toXml = undefined-}

{-instance (Processor a, Processor b) => Processor (Either a b) where-}
  {-type ProofObject (Either a b) = Either (ProofObject a) (ProofObject b)-}
  {-type Problem (Either a b)     = Either (Problem a) (Problem b)-}
  {-type Forking (Either a b)     = Exclusive (Forking a) (Forking b)-}
  {-name (Left p) = name p-}
  {-name (Right p) = name p-}
  {-solve (Right p) (Right prob) = do-}
    {-res <- solve p prob-}
    {-return $ case res of-}
      {-Fail a -> Fail (Right a)-}
      {-Success {} -> undefined-}
  {-solve (Left p) (Left prob) = do-}
    {-res <- solve p prob-}
    {-return $ case res of-}
      {-Fail a -> Fail (Left a)-}
      {-Success {} -> undefined-}
  {-solve _ _ = undefined-}

{-data Program = Program deriving Show-}
{-instance PP.Pretty Program where pretty = undefined-}
{-instance Xml.Xml Program where toXml = undefined-}

{-left :: p -> Either p (FailProcessor (TrsProblem Fun Var))-}
{-left = Left -}

{-right :: p -> Either (FailProcessor Program) p-}
{-right = Right -}

{-st :: Strategy (Either Program (TrsProblem Fun Var))-}
{-st =  pstrat (Right linear) >>> pstrat (right quadratic)-}

{-st2 = pstrat (Right linear)-}

-- or arrow
-- casting ??
-- class Problem p ~ prob => Processor p prob where
-- instance Processor p => Processor (P p)
--   type Problem (P p) = P (Problem p)a -- and P is ?
-- input ? output problem
-- instance Processor a prob => Processor a (Program prob) ??
