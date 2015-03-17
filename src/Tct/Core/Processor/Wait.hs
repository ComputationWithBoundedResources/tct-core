-- | This module provides the /Wait Processor/.
module Tct.Core.Processor.Wait 
  ( waitDeclaration
  , wait
  ) where


import           Data.Maybe             (fromMaybe)

import           Tct.Core.Data          hiding (wait)


-- | Wraps the application of a processor in a timeout.
data Wait prob = Wait
  { inW    :: Int
  , stratW :: Strategy prob }

instance Show (Wait prob) where
  show p = show (stratW p)

type WaitProof = ()


instance ProofData prob => Processor (Wait prob) where
  type ProofObject (Wait prob)   = WaitProof
  type Problem (Wait prob)       = prob

  solve p prob = do
    remainingM <- remainingTime `fmap` askStatus prob
    let pause = min (inW p) (inW p `fromMaybe` remainingM)
    paused pause (evaluate (stratW p) prob)

--- * instances ------------------------------------------------------------------------------------------------------

waitStrategy :: ProofData prob => Int -> Strategy prob -> Strategy prob
waitStrategy n st = Proc $ Wait { inW=n, stratW=st }

waitDeclaration :: ProofData prob => Declaration(
  '[ Argument 'Required Nat
   , Argument 'Required (Strategy prob) ]
  :-> Strategy prob)
waitDeclaration = declare "wait" help args waitStrategy
  where
    help = ["Pauses for <nat> seconds."]
    args = (waitForArg, strat)
    waitForArg = nat `withName` "for" `withHelp` [ "Pauses the computation <nat> seconds." ]

wait :: ProofData prob => Nat -> Strategy prob -> Strategy prob
wait = declFun waitDeclaration

