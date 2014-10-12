module Tct.Processors.Timeout 
  ( 
  -- * Strategy Declaration
  timeoutSD
  -- * Strategies
  , timeoutIn
  , timeoutUntil
  , timeoutRemaining
  ) where


import Data.Maybe (fromMaybe)

import Tct.Core
import Tct.Core.Declaration.Parse ()
import Tct.Processors.Failing
import qualified Tct.Common.Pretty as PP

instance Show p => Show (TimeoutProcessor p) where
  show (TimeoutProc mi mj p) = "timeout " ++ k mi ++ k mj ++ show p
    where k = maybe "" (\m -> show m ++ " ")

data TimeoutProof = Timeout Int

instance Show TimeoutProof where
  show (Timeout i)    = "Timeout " ++ show i

instance PP.Pretty TimeoutProof where
  pretty (Timeout i)    = PP.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")

{-instance Processor p => Xml.Xml (TimeoutProof p) where-}
  {-toXml (Timeout i)     = Xml.elt "timeout" [Xml.int i]-}

-- | Wraps the application of a processor in a timeout.
data TimeoutProcessor prob = TimeoutProc 
  { untilT :: Maybe Int
  , inT    :: Maybe Int
  , stratT :: Strategy prob }

instance ProofData prob => Processor (TimeoutProcessor prob) where
  type ProofObject (TimeoutProcessor prob)   = TimeoutProof
  type Problem (TimeoutProcessor prob)       = prob
  type ProcessorArgs (TimeoutProcessor prob) =
    '[ Argument 'Required (Maybe Nat)
     , Argument 'Required (Maybe Nat)
     , Argument 'Required (Strategy prob) ]
  declaration _ = declareProcessor "timeout" (some nat, some nat, strat) TimeoutProc

  solve proc prob = do
    running <- runningTime `fmap` askStatus prob
    let
      toNat n = case n of
        Just i | i >= 0 -> Just (toSeconds i)
        _               -> Nothing
      to = case (toNat $ inT proc, toNat $ untilT proc) of
        (Nothing, Just u ) -> max 0 (u - running)
        (Just i , Nothing) -> i
        (Just i , Just u ) -> min i (max 0 (u - running))
        _                  -> -1
    remains <- (fromMaybe to . toNat . remainingTime) `fmap` askStatus prob
    mr <- timeout (min to remains) (evaluate (stratT proc) prob)
    return $ case mr of
      Nothing -> resultToTree proc prob (Fail (Timeout to))
      Just r  -> r

toSeconds :: Int -> Int
toSeconds n = 1000000*n

-- Default 'TimeoutProcessor' instance.
timeoutProcessor :: ProofData prob => TimeoutProcessor prob
timeoutProcessor = TimeoutProc Nothing Nothing failing

-- | The timeout strategy declaration.
timeoutSD :: ProofData prob => StrategyDeclaration prob
timeoutSD = SD . liftP $ declaration timeoutProcessor

-- | @'timoutIn' i p@ aborts the application of @p@ after @min i 'remainingTime'@ seconds;
-- If @i@ is negative the processor may run forever.
timeoutIn :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutIn n = Proc . TimeoutProc (Just n) Nothing

-- | @'timeoutUntil' i p@ aborts the application of @p@ until i seconds wrt.
-- to the starting time, or if 'remainingTime' is expired.
-- If @i@ is negative the processor may run forever.
timeoutUntil :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutUntil n = Proc . TimeoutProc Nothing (Just $ toSeconds n)

-- | @'timeoutRemaining' i p@ sets the timeout to the 'remainingtime'.
timeoutRemaining :: ProofData prob => Strategy prob -> Strategy prob
timeoutRemaining = Proc . TimeoutProc Nothing Nothing

