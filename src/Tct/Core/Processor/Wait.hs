-- | This module provides the /Wait Processor/.
module Tct.Core.Processor.Wait
  ( waitDeclaration
  , wait
  ) where


import           Data.Maybe             (fromMaybe)

import           Tct.Core.Data          hiding (wait)


-- | Wraps the application of a processor in a timeout.
data Wait i o = Wait
  { inW    :: Int
  , stratW :: Strategy i o }

instance Show (Wait i o) where
  show p = show (stratW p)

type WaitProof = ()


instance (ProofData i, ProofData o) => Processor (Wait i o) where
  type ProofObject (Wait i o)   = WaitProof
  type I (Wait i o)       = i
  type O (Wait i o)       = o

  solve p prob = do
    remainingM <- remainingTime `fmap` askStatus prob
    let pause = min (inW p) (inW p `fromMaybe` remainingM)
    paused pause (evaluate (stratW p) prob)

--- * instances ------------------------------------------------------------------------------------------------------

waitStrategy :: (ProofData i, ProofData o) => Int -> Strategy i o -> Strategy i o
waitStrategy n st = Proc $ Wait { inW=n, stratW=st }

waitDeclaration :: (ProofData i, ProofData o) => Declaration(
  '[ Argument 'Required Nat
   , Argument 'Required (Strategy i o) ]
  :-> Strategy i o)
waitDeclaration = declare "wait" help args waitStrategy
  where
    help = ["Pauses for <nat> seconds."]
    args = (waitForArg, strat)
    waitForArg = nat `withName` "for" `withHelp` [ "Pauses the computation <nat> seconds." ]

wait :: (ProofData i, ProofData o) => Nat -> Strategy i o -> Strategy i o
wait = declFun waitDeclaration

