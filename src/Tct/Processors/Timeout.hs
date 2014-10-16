module Tct.Processors.Timeout 
  ( 
  -- * Declaration
  timeoutDeclaration
  -- * Strategies
  , timeout
  , timeoutIn
  , timeoutUntil
  , timeoutRemaining
  ) where

import Data.Maybe (fromMaybe)

import qualified Tct.Common.Pretty as PP
import Tct.Core
import Tct.Core.Declaration.Parse ()
import Tct.Processors.Failing


instance Show p => Show (TimeoutProcessor p) where
  show (TimeoutProc mi mj p) = "timeout " ++ k mi ++ k mj ++ show p
    where k = maybe "" (\m -> show m ++ " ")

data TimeoutProof = Timeout Int

instance Show TimeoutProof where
  show (Timeout i) = "Timeout " ++ show i

instance PP.Pretty TimeoutProof where
  pretty (Timeout i) = PP.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")

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
    '[ Argument 'Optional (Maybe Nat)
     , Argument 'Required (Maybe Nat)
     , Argument 'Required (Strategy prob) ]
  declaration _ = declareProcessor 
    "timeout" 
    [ "timeout m n s runs ..."  ]
    ( (some nat `optional` Nothing)
      `withName` "until"
      `withHelp` ["Aborts the computation "]
    , some nat
      `withName` "in"
      `withHelp` ["The timeout in seconds."]
    , strat 
      `withName` "strategy"
      `withHelp` ["The strategy to apply with timeout."]) 
    $ \n m s -> Proc (TimeoutProc n m s)

  solve proc prob = do
    running <- runningTime `fmap` askStatus prob
    let
      toNat n = case n of
        Just i | i >= 0 -> Just (1000000*i)
        _               -> Nothing
      to = case (toNat $ inT proc, toNat $ untilT proc) of
        (Nothing, Just u ) -> max 0 (u - running)
        (Just i , Nothing) -> i
        (Just i , Just u ) -> min i (max 0 (u - running))
        _                  -> -1
    remains <- (fromMaybe to . toNat . remainingTime) `fmap` askStatus prob
    mr <- timeit (min to remains) (evaluate (stratT proc) prob)
    return $ case mr of
      Nothing -> resultToTree proc prob (Fail (Timeout to))
      Just r  -> r


-- Standard timeout processor
timeoutProcessor :: ProofData prob => TimeoutProcessor prob
timeoutProcessor = TimeoutProc Nothing Nothing failing

-- | TimoutProcessor declaration.
timeoutDeclaration :: ProofData prob => Declaration(
  '[ Argument 'Optional (Maybe Nat)
   , Argument 'Required (Maybe Nat)
   , Argument 'Required (Strategy prob) ]
  :-> Strategy prob)
timeoutDeclaration = declaration $ timeoutProcessor


-- Strategies --------------------------------------------------------------------------------------------------------

-- | prop> timeout m n s = timeoutUntil m (timoutIn n s) = timeoutIn n (timeoutUntil m s)
timeout :: ProofData prob => Int -> Int -> Strategy prob -> Strategy prob
timeout n m = Proc . TimeoutProc (Just n) (Just m)

-- | @'timoutIn' i p@ aborts the application of @p@ after @min i 'remainingTime'@ seconds;
-- If @i@ is negative the processor may run forever.
timeoutIn :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutIn n = Proc . TimeoutProc Nothing (Just n)

-- | @'timeoutUntil' i p@ aborts the application of @p@ until i seconds wrt.
-- to the starting time, or if 'remainingTime' is expired.
-- If @i@ is negative the processor may run forever.
timeoutUntil :: ProofData prob => Int -> Strategy prob -> Strategy prob
timeoutUntil n = Proc . TimeoutProc (Just n) Nothing

-- | @'timeoutRemaining' i p@ sets the timeout to the 'remainingtime'.
timeoutRemaining :: ProofData prob => Strategy prob -> Strategy prob
timeoutRemaining = Proc . TimeoutProc Nothing Nothing

