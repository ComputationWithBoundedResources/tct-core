module Tct.Core.Processor.Timeout 
  ( 
  -- * Timeout
  -- ** Declaration
  timeout
  , timeoutDeclaration
  -- ** Strategies
  , timeoutIn
  , timeoutUntil
  , timeoutRemaining

  -- * Wait
  , wait
  , waitDeclaration
  ) where


import           Data.Maybe             (fromMaybe)

import qualified Tct.Core.Common.Pretty as PP
import           Tct.Core.Data          hiding (wait)


instance Show p => Show (TimeoutProcessor p) where
  show (TimeoutProc mi mj p) = "timeout " ++ k mi ++ k mj ++ show p
    where k = maybe "" (\m -> show m ++ " ")

data TimeoutProof = Timeout Int

instance Show TimeoutProof where
  show (Timeout i) = "Timeout " ++ show i

instance PP.Pretty TimeoutProof where
  pretty (Timeout i) = PP.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")

-- | Wraps the application of a processor in a timeout.
data TimeoutProcessor prob = TimeoutProc 
  { untilT :: Maybe Int
  , inT    :: Maybe Int
  , stratT :: Strategy prob }

instance ProofData prob => Processor (TimeoutProcessor prob) where
  type ProofObject (TimeoutProcessor prob)   = TimeoutProof
  type Problem (TimeoutProcessor prob)       = prob

  solve proc prob = do
    running <- runningTime `fmap` askStatus prob
    let
      to = case (toNat (inT proc), toNat (untilT proc)) of
        (Nothing, Just u ) -> max 0 (u - running)
        (Just i , Nothing) -> i
        (Just i , Just u ) -> min i (max 0 (u - running))
        _                  -> -1
    remains <- (fromMaybe to . toNat . remainingTime) `fmap` askStatus prob
    let actual = min to (cutoff remains delta)
    mr <- timed  actual (evaluate (stratT proc) prob)
    return $ case mr of
      Nothing -> resultToTree proc prob (Fail (Timeout to))
      Just r  -> r
    where
      toNat n = case n of
        Just i | i >= 0 -> Just i
        _               -> Nothing
      cutoff a b = max 0 (a -b)
      delta = 1 :: Int 


-- | prop> timeout m n st = timeoutUntil m (timoutIn n st) = timeoutIn n (timeoutUntil m st)
timeout :: ProofData prob => Maybe Int -> Maybe Int -> Strategy prob -> Strategy prob
timeout n m = Proc . TimeoutProc n m 

timeoutInArg :: Argument 'Required Int
timeoutInArg = nat `withName` "in" `withHelp` ["Aborts the comutation in <nat> seconds."]

timeoutUntilArg :: Argument 'Required Int
timeoutUntilArg = nat 
  `withName` "until" 
  `withHelp` ["Aborts the comutation after <nat> seconds wrt. the starting time."]

-- | TimoutProcessor declaration.
-- 
--   * Each application of the timeout processor, sets 'remainingTime' for the sub-computation.
--   * A timeout is maximal 'remainingTime'.
--   * Nothing is treated as no timeout.
timeoutDeclaration :: ProofData prob => Declaration(
  '[ Argument 'Optional (Maybe Nat)
   , Argument 'Required (Maybe Nat)
   , Argument 'Required (Strategy prob) ]
  :-> Strategy prob)
timeoutDeclaration = declare "timeout" help args timeout
  where
    help = ["Wraps the computation in a timeout."]
    args = (some timeoutUntilArg `optional` Nothing, some timeoutInArg, strat)

-- * Wait
instance Show (WaitProcessor prob) where
  show p = show (stratW p)

type WaitProof = ()

-- | Wraps the application of a processor in a timeout.
data WaitProcessor prob = WaitProc
  { inW    :: Int
  , stratW :: Strategy prob }

instance ProofData prob => Processor (WaitProcessor prob) where
  type ProofObject (WaitProcessor prob)   = WaitProof
  type Problem (WaitProcessor prob)       = prob
  solve p prob = do
    remainingM <- remainingTime `fmap` askStatus prob
    let pause = min (inW p) (inW p `fromMaybe` remainingM)
    paused pause (evaluate (stratW p) prob)

wait :: ProofData prob => Int -> Strategy prob -> Strategy prob
wait n m = Proc $ WaitProc n m

waitForArg :: Argument 'Required Nat
waitForArg = nat `withName` "for" `withHelp` ["Pauses the computation <nat> seconds."]

waitDeclaration :: ProofData prob => Declaration(
  '[ Argument 'Required Nat
   , Argument 'Required (Strategy prob) ]
  :-> Strategy prob)
waitDeclaration = declare "wait" help args wait
  where
    help = ["Pauses for <nat> seconds."]
    args = (waitForArg, strat)


-- Strategies --------------------------------------------------------------------------------------------------------

-- | @'timoutIn' i st@ aborts the application of @st@ after @min i 'remainingTime'@ seconds;
--
-- prop> i < 0 => timeoutIn i = timeout Nothing Nothing
timeoutIn :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutIn n = Proc . TimeoutProc Nothing (Just n)

-- | @'timeoutUntil' i st@ aborts the application of @st@ after i seconds wrt.
-- to the starting time, or if 'remainingTime' is expired.
--
-- prop> i < 0 => timeoutUntil i = timeout Nothing Nothing
timeoutUntil :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutUntil n = Proc . TimeoutProc (Just n) Nothing

-- | @'timeoutRemaining' i p@ sets the timeout to the 'remainingtime'.
--
-- prop> timeoutRemaining = timeout Nothing Nothing
timeoutRemaining :: ProofData prob => Strategy prob -> Strategy prob
timeoutRemaining = Proc . TimeoutProc Nothing Nothing

