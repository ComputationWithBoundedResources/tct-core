-- | This module provides common 'Strategy' and 'Processor' combinators.
module Tct.Combinators
  (
  -- * Strategy Combinators
  -- | We define right-associative infix versions of 'Strategy' constructors.
  -- Following precedence holds: optionally,stateful > alternative > sequential.
  -- For example,
  --
  -- prop> try s1 >>> s2 >||> s3 <> s4 >>> s5 = (try s1) >>> (s2 >||> ((s3 <> s4) >>> s5))
  --
  -- We say that a @'Strategy' s@
  --
  --  * is continuing if the evaluation returns @'Continue' pt@,
  --  * is progressing if additionally @'progress' pt = 'True'@,
  --  * is failing if it is not continuing.
  some
  , NList (..)
  -- ** Sequential
  , (>>>), (>||>)
  , chain
  -- ** Alternative
  , (<>), (<||>), (<?>)
  , alternative
  , fastest
  , best
  -- ** Optional
  , try, force
  -- ** Stateful
  , withState
  , withProblem

  -- ** Combinators
  , when
  , exhaustively

  -- * Processor Combinators

  -- ** Trivial Combinators
  , FailProcessor, abort
  , NamedProcessor, named

  -- ** Time
  , TimeoutProcessor, timeoutIn, timeoutUntil

  -- * Processor List
  , processors
  )

where

import           Control.Applicative (optional, (<$>), (<*>))
import           Prelude

import           Tct.Common.Options
import qualified Tct.Common.Pretty   as PP
import           Tct.Core            as C


-- | List of default processor combinators.
-- 'SomeProcessor' are parsable and provide a description.
processors :: ProofData prob => [SomeProcessor prob]
processors =
  [ SomeProc failProcessor
  , SomeProc namedProcessor
  , SomeProc timeoutProcessor
  ]


-- Strategy Combinators ------------------------------------------------------------------------------------------------

-- | Lifts a processor to a 'Strategy'.
some :: (Processor p, ParsableProcessor p)  => p ->  Strategy (Problem p)
some = Proc . SomeProc

infixr 5 >>>, >||>
infixr 6 <>, <||>

-- | Infix version of 'Then'.
-- @s1 '>>>' s2@ applies @s1@ before @s2@.
-- Fails if @s1@ or @s2@ fails.
--
-- prop> s1 >>> (s2 >>> s3) = (s1 >>> s2) >>> s3 = s1 >>> s2 >>> s3
(>>>) :: Strategy prob -> Strategy prob -> Strategy prob
(>>>)  = Then

-- | Infix version of 'ThenPar'.
-- Like ('>>>') but applies s2 on all problems in parallel.
(>||>) :: Strategy prob -> Strategy prob -> Strategy prob
(>||>) = ThenPar


-- | Infix version of 'Alt'.
-- @s1 '<>' s2@
--
-- * returns the result of @s1@ if @s1@ is progressing,
-- * returns the result of @s1@ if @s1@ is continuing and @s2@ is failing
-- * returns the result of @s2@ otherwise.
-- * Fails if @s1@ and @s2@ fails.
--
-- prop> s1 <> (s2 <> s3) = (s1 <> s2) <> s3 = s1 <> s2 <> s3
(<>):: Strategy prob -> Strategy prob -> Strategy prob
(<>)  = Alt

-- | Infix version of 'OrFaster'.
-- Behaves like ('<>') but applies the strategies in parallel.
-- Suppose that @s2@ ends before @s1@ then:
--
-- prop> s1 <||> s2  = s2 <> s1
(<||>) :: Strategy prob -> Strategy prob -> Strategy prob
(<||>) = OrFaster

-- | Alternative version of 'OrBetter'.
--
-- @('<?>') cmp s1 s2@ applies @'timeout' n s1@ and @'timeout' n s2@ in parallel and waits until both strategies have
-- finished. Here @n@ depens on 'remainingTime'. We consider the following cases:
--
--  * Both strategies end before the timeout
--
--     * and both strategies are progressing: we return @pt2@ only if @pt2 > pt1@ wrt to @cmp@, where @pt1@ and @pt2@
--       are the results of @s1@ and @s2@.
--     * otherwise it behaves like @s1 '<>' s2@.
--
--  * Only one strategy ends before the timeout, its result is returned.
--  * None of the stratgies end before the timeout, it fails.
--
-- An example implementation of cmp is:
--
-- > cmp pt1 pt2 = compare (timeUB $ certificate pt1) (timeUB $ certificate pt2)
(<?>) :: (ProofTree prob -> ProofTree prob -> Ordering) -> Strategy prob -> Strategy prob -> Strategy prob
(<?>) = OrBetter

-- | @'try' s@ is continuing even if @s@ is not.
--
-- prop> try (force s) = try s
try :: Strategy prob -> Strategy prob
try s@(Trying _ _) = Trying True s
try (WithStatus f) = WithStatus (try . f)
try s              = Trying True s

-- | @'force' s@ fails if @s@ is not progressing.
--
-- prop> force (try s) = force s
force :: Strategy prob -> Strategy prob
force s@(Trying _ _) = Trying False s
force (WithStatus f) = WithStatus (force . f)
force s              = Trying False s

-- | prop> withState = 'WithStatus'
withState :: (TctStatus prob -> Strategy prob) -> Strategy prob
withState = WithStatus

-- | Specialised version of 'withState'.
withProblem :: (prob -> Strategy prob) -> Strategy prob
withProblem g = WithStatus (g . currentProblem)

-- | Defines a non-empty list.
data NList a = a :| [a] deriving (Eq, Ord, Show)

-- | List version of ('>>>').
chain :: NList (Strategy prob) -> Strategy prob
chain (s:|ss) = foldr1 (>>>) (s:ss)

-- | List version of ('<>').
alternative :: NList (Strategy prob) -> Strategy prob
alternative (s:|ss) = foldr1 (<>) (s:ss)

-- | List version of ('<||>').
fastest :: NList (Strategy prob) -> Strategy prob
fastest (s:|ss) = foldr1 (<||>) (s:ss)

-- | List version of ('<?>').
best :: (ProofTree prob -> ProofTree prob -> Ordering) -> NList (Strategy prob) -> Strategy prob
best cmp (s:|ss) = foldr1 (cmp <?>) (s:ss)


-- | @exhaustively s@ repeatedly applies @s@ until @s@ fails.
-- Fails if the first application of @s@ fails.
exhaustively :: Strategy prob -> Strategy prob
exhaustively s =  s >>> try (exhaustively s)

-- | @'when' s then@ applies @then@ if @s@ is progressing. Never fails.
when :: Strategy prob -> Strategy prob -> Strategy prob
when s sthen = try $ force s >>> sthen
--whenNot = try $ force s <> sthen


-- Processor Combinators ------------------------------------------------------

-- * Fail Processor -----------------------------------------------------------
-- | A processor that always fails.
-- Does not necessarily abort the evaluation.
data FailProcessor prob = FailProc deriving Show

data FailProof = FailProof deriving Show

instance PP.Pretty FailProof where
  pretty _ = PP.paragraph "We apply FailProccessor."
instance ProofData prob => Processor (FailProcessor prob) where
  type ProofObject (FailProcessor prob) = FailProof
  type Problem (FailProcessor prob) = prob
  name _ = "FailProcessor"
  solve _ _ = return $ Fail FailProof

instance ProofData prob => ParsableProcessor (FailProcessor prob) where

-- | Default 'FailProcessor' instance.
failProcessor :: FailProcessor prob
failProcessor = FailProc

-- | @abort@ always return 'Fail' and therfore 'Abort'.
-- Does not necessarily abort the whole evaluation.
abort :: FailProcessor prob
abort = FailProc


-- * Named Procesor -----------------------------------------------------------
-- | Gives a dedicated name to a processor. Useful for pretty-printing and parser generation.
data NamedProcessor p 
  = NamedProc String p deriving Show

data NamedProof p 
  = NamedProof String (ProofObject p)

instance Processor p => Show (NamedProof p) where
  show (NamedProof n po) = n ++ "(" ++ show po ++ ")"

instance Processor p => PP.Pretty (NamedProof p) where
  pretty (NamedProof n po) = PP.paragraph ("We apply " ++ n ++ ":") PP.<$$> PP.pretty po

instance Processor p => Processor (NamedProcessor p) where
  type ProofObject (NamedProcessor p) = NamedProof p
  type Forking (NamedProcessor p)     = Forking p
  type Problem (NamedProcessor p)     = Problem p
  name (NamedProc n _) = n
  solve (NamedProc n p) prob = do
    r1 <- solve p prob
    return $ case r1 of
      Success probs po certfn -> Success probs (NamedProof n po) certfn
      Fail po                 -> Fail $ NamedProof n po

instance Processor p => ParsableProcessor (NamedProcessor p) where
  args _ ps = argsParser pargs desc
    where
      pargs = NamedProc
        <$> argument Just (eopt
            `withMetavar` "name"
            `withHelpDoc` PP.paragraph "The name of the processor.")
        <*> argument (parseSomeProcessorMaybe ps) (eopt
            `withMetavar` "proc"
            `withHelpDoc` PP.string "The applied subprocessor.")
      desc = PP.string "NameProcessor"

namedProcessor :: NamedProcessor (FailProcessor prob)
namedProcessor = NamedProc "Failing" FailProc

-- | @'named' name p@ is like @p@. But provides a parser using @name@.
named :: String -> p -> NamedProcessor p
named = NamedProc


-- * Timeout Processor --------------------------------------------------------
-- | Wraps the application of a processor in a timeout.
data TimeoutProcessor p
  = TimeoutProc { untilT :: Maybe Int, inT :: Maybe Int, procT :: p }
  deriving Show

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

-- | Default 'TimeoutProcessor' instance.
timeoutProcessor :: TimeoutProcessor (FailProcessor prob)
timeoutProcessor = TimeoutProc Nothing Nothing failProcessor

-- | @timoutIn i p@ aborts the application of @p@ after @i@ seconds.
-- If @i@ is negative the processor may run forever.
timeoutIn :: Int -> p -> TimeoutProcessor p
timeoutIn n = TimeoutProc (Just n) Nothing

-- | @timeoutUntil i p@ aborts the application of @p@ until i seconds wrt. to the starting time.
-- If @i@ is negative the processor may run forever.
timeoutUntil :: Int -> p -> TimeoutProcessor p
timeoutUntil n = TimeoutProc Nothing (Just n)

