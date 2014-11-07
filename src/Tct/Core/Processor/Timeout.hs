module Tct.Core.Processor.Timeout 
  ( 
  -- * Timeout
  -- ** Declaration
  timeoutDeclaration
  -- ** Strategies
  , timeout
  , timeoutIn
  , timeoutUntil
  , timeoutRemaining
  ) where


import           Data.Maybe                      (fromMaybe)

import qualified Tct.Core.Common.Pretty          as PP
import           Tct.Core.Data
import           Tct.Core.Common.Error (liftIO)
import           Tct.Core.Processor.Trivial


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
    [ "Wrappes the computation in a timeout."  ]
    ( (some nat `optional` Nothing)
      `withName` "until"
      `withHelp` ["Aborts the computation after until <nat> seconds, wrt. the starting time. "]
    , some nat
      `withName` "in"
      `withHelp` ["Aborts the comutation in <nat> seconds."]
    , strat )
    $ \n m s -> Proc (TimeoutProc n m s)

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

-- Standard timeout processor
timeoutProcessor :: ProofData prob => TimeoutProcessor prob
timeoutProcessor = TimeoutProc Nothing Nothing failing

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
timeoutDeclaration = declaration $ timeoutProcessor


-- Strategies --------------------------------------------------------------------------------------------------------

-- | prop> timeout m n st = timeoutUntil m (timoutIn n st) = timeoutIn n (timeoutUntil m st)
timeout :: ProofData prob => Int -> Int -> Strategy prob -> Strategy prob
timeout n m = Proc . TimeoutProc (Just n) (Just m)

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

