-- | This module provides the /Timeout Processor/.
module Tct.Core.Processor.Timeout
  ( timeoutInDeclaration
  , timeoutIn

  , timeoutUntilDeclaration
  , timeoutUntil

  , timeoutRemaining
  , timeoutRelative
  ) where


import           Data.Maybe             (fromMaybe)

import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Xml    as Xml
import           Tct.Core.Data          hiding (wait)


data Timeout i o
  = TimeoutIn
    { time_       :: Int
    , onStrategy_ :: Strategy i o }
  | TimeoutUntil
    { time_       :: Int
    , onStrategy_ :: Strategy i o }
  deriving Show

data TimeoutProof = TimeoutProof Int

instance ProofData i => Processor (Timeout i o) where
  type ProofObject (Timeout i o) = TimeoutProof
  type I (Timeout i o)           = i
  type O (Timeout i o)           = o

  solve proc prob = do
    running <- runningTime `fmap` askStatus prob
    let
      to = case proc of
        TimeoutIn{}    -> max 0 (time_ proc)
        TimeoutUntil{} -> max 0 $ (time_ proc) - running

    remains <- (fromMaybe to . remainingTime) `fmap` askStatus prob
    let actual = min to (cutoff remains delta)

    mr <- timed actual (evaluate (onStrategy_ proc) prob)
    return $ case mr of
      Nothing -> resultToTree' proc prob $ Fail (TimeoutProof to)
      Just r  -> r

    where
      cutoff a b = max 0 (a -b)
      delta = 1 :: Int


--- * instances ------------------------------------------------------------------------------------------------------

timeoutInStrategy, timeoutUntilStrategy :: ProofData i => Int -> Strategy i o -> Strategy i o
timeoutInStrategy i st    = toStrategy $ TimeoutIn {time_ = i, onStrategy_ = st}
timeoutUntilStrategy u st = toStrategy $ TimeoutUntil {time_ = u, onStrategy_ = st}

description :: [String]
description = ["Wraps the computation in a timeout."]

-- | TimoutProcessor declaration.
--
--   * Each application of the timeout processor, sets 'remainingTime' for the sub-computation, via 'timed'.
--   * A timeout is maximal the 'remainingTime'.
timeoutInDeclaration, timeoutUntilDeclaration :: ProofData i => Declaration(
  '[ Argument 'Required Nat
   , Argument 'Required (Strategy i o) ]
  :-> Strategy i o )
timeoutInDeclaration = declare "timeoutIn" description (timeArg, strat) timeoutInStrategy where
  timeArg = nat
    `withName` "in"
    `withHelp` ["Aborts the comutation in <nat> seconds."]
timeoutUntilDeclaration = declare "timeoutUntil" description (timeArg, strat) timeoutUntilStrategy where
  timeArg = nat
    `withName` "until"
    `withHelp` ["Aborts the comutation after <nat> seconds."]

-- | @'timoutIn' i st@ aborts the application of @st@ after @min i 'remainingTime'@ seconds;
timeoutIn :: ProofData i => Int -> Strategy i o -> Strategy i o
timeoutIn = declFun timeoutInDeclaration

-- | @'timeoutUntil' i st@ aborts the application of @st@ after i seconds wrt.
-- to the starting time, or if 'remainingTime' is expired.
timeoutUntil :: ProofData i => Int -> Strategy i o -> Strategy i o
timeoutUntil = declFun timeoutUntilDeclaration

-- | @'timeoutRemaining' i p@ sets the timeout to the 'remainingtime'.
timeoutRemaining :: ProofData i => Strategy i o -> Strategy i o
timeoutRemaining st = WithStatus $ \ state -> maybe st (flip timeoutIn st) (remainingTime state)


-- | Variant of 'timeoutRemainting' which sets timeout relative to the given percentage.
-- So @timeoutRelative 50 st@ sets the timeout half to the remaining time.
timeoutRelative :: ProofData i => Int -> Strategy i o -> Strategy i o
timeoutRelative i st = WithStatus $ \ state -> maybe st timeout (remainingTime state)
  where timeout j = timeoutIn (floor $ (fromIntegral (i*j :: Int) / 100 :: Double)) st
 
--- * proofdata ------------------------------------------------------------------------------------------------------

instance Show TimeoutProof where
  show (TimeoutProof i) = "Timeout " ++ show i

instance PP.Pretty TimeoutProof where
  pretty (TimeoutProof i) = PP.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")

instance Xml.Xml TimeoutProof where
  toXml (TimeoutProof i) = Xml.elt "timeout" [Xml.int i]

