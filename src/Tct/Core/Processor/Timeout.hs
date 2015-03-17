-- | This module provides the /Timeout Processor/.
module Tct.Core.Processor.Timeout
  ( timeoutDeclaration
  , timeout
  , timeout'

  , timeoutIn
  , timeoutUntil
  , timeoutRemaining
  ) where


import           Data.Maybe             (fromMaybe)

import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml    as Xml
import           Tct.Core.Data          hiding (wait)


-- | Wraps the application of a processor in a timeout.
data Timeout prob = Timeout
  { untilT :: Maybe Int
  , inT    :: Maybe Int
  , stratT :: Strategy prob }

instance Show p => Show (Timeout p) where
  show (Timeout mi mj p) = "timeout " ++ k mi ++ k mj ++ show p
    where k = maybe "" (\m -> show m ++ " ")

data TimeoutProof = TimeoutProof Int


instance ProofData prob => Processor (Timeout prob) where
  type ProofObject (Timeout prob)   = TimeoutProof
  type Problem (Timeout prob)       = prob

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
      Nothing -> resultToTree proc prob (Fail (TimeoutProof to))
      Just r  -> r
    where
      toNat n = case n of
        Just i | i >= 0 -> Just i
        _               -> Nothing
      cutoff a b = max 0 (a -b)
      delta = 1 :: Int


--- * instances ------------------------------------------------------------------------------------------------------

timeoutStrategy :: ProofData prob => Maybe Int -> Maybe Int -> Strategy prob -> Strategy prob
timeoutStrategy ut it st = Proc $ Timeout { untilT=ut, inT=it, stratT=st }

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
timeoutDeclaration = declare "timeout" help args timeoutStrategy
  where
    help = ["Wraps the computation in a timeout."]
    args = (some timeoutUntilArg `optional` Nothing, some timeoutInArg, strat)
    timeoutInArg = nat 
      `withName` "in" 
      `withHelp` ["Aborts the comutation in <nat> seconds."]
    timeoutUntilArg = nat 
      `withName` "until" 
      `withHelp` ["Aborts the comutation after <nat> seconds wrt. the starting time."]

-- | prop> timeout m n st = timeoutUntil m (timoutIn n st) = timeoutIn n (timeoutUntil m st)
timeout :: ProofData prob => Maybe Int -> Maybe Int -> Strategy prob -> Strategy prob
timeout = declFun timeoutDeclaration

timeout' :: ProofData prob => Maybe Int -> Strategy prob -> Strategy prob
timeout' = deflFun timeoutDeclaration


-- | @'timoutIn' i st@ aborts the application of @st@ after @min i 'remainingTime'@ seconds;
--
-- prop> i < 0 => timeoutIn i = timeout Nothing Nothing
timeoutIn :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutIn n = Proc . Timeout Nothing (Just n)

-- | @'timeoutUntil' i st@ aborts the application of @st@ after i seconds wrt.
-- to the starting time, or if 'remainingTime' is expired.
--
-- prop> i < 0 => timeoutUntil i = timeout Nothing Nothing
timeoutUntil :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutUntil n = Proc . Timeout (Just n) Nothing

-- | @'timeoutRemaining' i p@ sets the timeout to the 'remainingtime'.
--
-- prop> timeoutRemaining = timeout Nothing Nothing
timeoutRemaining :: ProofData prob => Strategy prob -> Strategy prob
timeoutRemaining = Proc . Timeout Nothing Nothing


--- * proofdata ------------------------------------------------------------------------------------------------------

instance Show TimeoutProof where
  show (TimeoutProof i) = "Timeout " ++ show i

instance PP.Pretty TimeoutProof where
  pretty (TimeoutProof i) = PP.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")

instance Xml.Xml TimeoutProof where
  toXml (TimeoutProof i) = Xml.elt "timeout" [Xml.int i]

