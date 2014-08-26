module Tct.Combinators
  (
    -- * Strategy Combinators
    some
  , (>>>), (>||>), (<|>), (<?>), (<||>)
  , try, force
  , withState

  , NList (..)
  , sequence
  , alternative
  , fastest
  , best

  , exhaustively
  , ite

    -- * Processor Combinators

    -- ** Trivial Combinators

  , FailProcessor (..)

    -- ** Time
  , TimeoutProcessor (..)
  , timeoutIn
  , timeoutUntil

  )

where

import           Control.Applicative hiding (some, (<|>))
import           Prelude             hiding (sequence)

import           Tct.Core            as C
import           Tct.Common.Options
import qualified Tct.Common.Pretty          as PP
-- import Tct.Xml as Xml



-- Strategy Combinators ------------------------------------------------------

-- TODO: check associativity
-- `Then` etc have infixl 9
-- remove <?> with default cmp
-- some conditioinal on prooftree is missing; to express fexample ifprogress

infixr 6 >>>, >||>
infixr 6 <|>, <||>

(>>>), (>||>), (<|>), (<||>) :: Strategy prob -> Strategy prob -> Strategy prob
(>>>)  = Then
(>||>) = ThenPar
(<|>)  = Alt
(<||>) = OrFaster

(<?>) :: (ProofTree prob -> ProofTree prob -> Ordering) -> Strategy prob -> Strategy prob -> Strategy prob
(<?>) = OrBetter

some :: (Processor p, ParsableProcessor p)  => p ->  Strategy (Problem p)
some = Proc . SomeProc

try :: Strategy prob -> Strategy prob
try s@(Trying _ _) = Trying True s
try (WithStatus f) = WithStatus (try . f)
try s              = Trying True s

force :: Strategy prob -> Strategy prob
force s@(Trying _ _) = Trying False s
force (WithStatus f) = WithStatus (force . f)
force s              = Trying False s

withState :: (TctStatus prob -> Strategy prob) -> Strategy prob
withState = WithStatus


-- list version
data NList a = a :| [a] deriving Show

sequence :: NList (Strategy prob) -> Strategy prob
sequence (s:|ss) = foldr1 (>>>) (s:ss)

alternative :: NList (Strategy prob) -> Strategy prob
alternative (s:|ss) = foldr1 (<|>) (s:ss)

fastest :: NList (Strategy prob) -> Strategy prob
fastest (s:|ss) = foldr1 (<||>) (s:ss)

best :: (ProofTree prob -> ProofTree prob -> Ordering) -> NList (Strategy prob) -> Strategy prob
best cmp (s:|ss) = foldr1 (cmp <?>) (s:ss)


exhaustively :: Strategy prob -> Strategy prob
exhaustively s =  s >>> try (exhaustively s)

ite :: Strategy prob -> Strategy prob -> Strategy prob -> Strategy prob
ite s sthen selse = s >>> try sthen <|> selse


-- Trivial Combinators -------------------------------------------------------

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


data NamedProcessor p = NamedProc String p deriving Show
data NamedProof p = NamedProof String (ProofObject p)

instance Processor p => Show (NamedProof p) where
  show (NamedProof st po) = st ++ "(" ++ show po ++ ")"
instance Processor p => PP.Pretty (NamedProof p) where
  pretty (NamedProof nm po) = PP.string nm PP.<$$> PP.indent 2 (PP.pretty po)

instance Processor p => Processor (NamedProcessor p) where
  type ProofObject (NamedProcessor p) = NamedProof p
  type Forking (NamedProcessor p) = Forking p
  type Problem (NamedProcessor p) = Problem p
  name (NamedProc nm _) = nm
  solve (NamedProc nm p) prob = do
    res <- solve p prob
    case res of
      Success fn po certfn -> return $ Success fn (NamedProof nm po) certfn
      Fail po              -> return $ Fail (NamedProof nm po)



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
        <$> optional (option $ eopt
            `withArgLong` "untilT"
            `withMetavar` "iSec"
            `withHelpDoc` PP.paragraph "Aborts the computation after 'iSec' from the startint time.")
        <*> optional (option $ eopt
            `withArgLong` "inT"
            `withMetavar` "iSec"
            `withHelpDoc` PP.paragraph "Aborts the computation after 'iSec' from starting the sub processor.")
        <*> argument  (parseSomeProcessorMaybe ps) (eopt
            `withMetavar` "proc"
            `withHelpDoc` PP.string "The applied subprocessor.")
      desc = PP.string "the timeoutprocessor"

timeoutIn :: Int -> p -> TimeoutProcessor p
timeoutIn n = TimeoutProc (Just n) Nothing

timeoutUntil :: Int -> p -> TimeoutProcessor p
timeoutUntil n = TimeoutProc Nothing (Just n)

