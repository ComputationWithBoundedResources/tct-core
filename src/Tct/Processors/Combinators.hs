module Tct.Processors.Combinators
  (
    -- * Trivial Combinators

    FailProcessor (..)

    -- * Combinators

    -- ** Timeout
  , TimeoutProcessor (..)
  , timeoutIn
  , timeoutUntil

    -- * Strategy Combinators
  , exhaustively
  )

where

import           Control.Applicative

import           Tct.Core            as C
import           Tct.Options
import qualified Tct.Pretty          as PP
-- import Tct.Xml as Xml


data FailProcessor prob = FailProc deriving Show
data FailProof = FailProof deriving Show
instance PP.Pretty FailProof where
  pretty _ = PP.paragraph "We apply FailProccessor."
instance ProofData prob => Processor (FailProcessor prob) where
  type ProofObject (FailProcessor prob) = FailProof
  type Forking (FailProcessor prob) = Judgement
  type Problem (FailProcessor prob) = prob
  name _ = "FailProcessor"
  solve _ _ = return $ Fail FailProof

instance ProofData prob => ParsableProcessor (FailProcessor prob) where


data TimeoutProcessor p = TimeoutProc { untilT :: Maybe Int, inT :: Maybe Int, procT :: p } deriving Show

data TimeoutProof p
  = Timeout Int
  | NoTimeout (ProofObject p)

instance Processor p => Show (TimeoutProof p) where
  show (Timeout i)    = "Timeout " ++ show i
  show (NoTimeout po) = "NoTimeout (" ++ show po ++ ")"

instance Processor p => PP.Pretty (TimeoutProof p) where
  pretty (Timeout i)    = PP.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")
  pretty (NoTimeout po) = PP.pretty po

instance Processor p => Processor (TimeoutProcessor p) where
  type ProofObject (TimeoutProcessor p) = TimeoutProof p
  type Forking (TimeoutProcessor p)     = Forking p
  type Problem (TimeoutProcessor p)     = Problem p
  name _ = "TimeoutProcessor"
  solve proc prob = do
    running <- runningTime `fmap` askStatus prob
    let
      t = case (inT proc, untilT proc) of
        (Nothing, Just u ) -> max 0 (u - running)
        (Just i , Nothing) -> max 0 i
        (Just i , Just u ) -> max 0 (min i (max 0 (u - running)))
        _                  -> 0
    mr <- timeout t (solve (procT proc) prob)
    return $ case mr of
      Nothing       -> Fail (Timeout t)
      Just (Fail p) -> Fail (NoTimeout p)
      Just r@(Success {}) -> Success
        { subProblems   = subProblems r
        , proofData     = NoTimeout (proofData r)
        , certificateFn = certificateFn r }

instance Processor p => ParsableProcessor (TimeoutProcessor p) where
  args _ ps = argsParser pargs desc
    where 
      pargs = TimeoutProc
        <$> optional (anyArg "untilT" "iSec" (PP.string "stops when ..") (-1))
        <*> optional (anyArg "inT" "iSec" (PP.string "aborts after iSec seconds") (-1))
        <*> arguArg (parseSomeProcessorMaybe ps) "proc" (PP.string "the applied subprocessor")
      desc = PP.string "the timeoutprocessor"

timeoutIn :: Int -> p -> TimeoutProcessor p
timeoutIn n = TimeoutProc (Just n) Nothing

timeoutUntil :: Int -> p -> TimeoutProcessor p
timeoutUntil n = TimeoutProc Nothing (Just n)

-- StrategyCombinator
exhaustively :: Strategy prob -> Strategy prob
exhaustively s =  s >>> try (exhaustively s)

