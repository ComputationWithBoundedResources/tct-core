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
  strat
  , pstrat
  , NList (..)
  , ProcessorStrategy (..)
  , Strategic
  -- ** Sequential
  , (>>>), (>||>), (>=>)
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

  -- ** Time
  , TimeoutProcessor, timeoutIn, timeoutUntil

  -- * Processor List
  , parsableProcessors
  )

where

import           Control.Applicative (optional, (<$>), (<*>))
import           Data.Maybe          (fromMaybe)
import           Tct.Common.Options
import qualified Tct.Common.Pretty   as PP
import           Tct.Core            as C


-- | List of default parsable processors and processor combinators.
parsableProcessors :: ProofData prob => [SomeParsableProcessor prob]
parsableProcessors =
  [ SomeParsableProc failProcessor
  , SomeParsableProc timeoutProcessor
  ]

-- | Newtype wrapper for processors.
-- If @p@ is a 'Processor' then @'ProcessorStrategy' p@ behaves like @p@.
-- Additionally, @'ProcessorStrategy' p@ is an instance of 'Strategic'.
newtype ProcessorStrategy a = ProcessorStrategy {fromProcessorStrategy :: a}

instance Show a => Show (ProcessorStrategy a) where
  show = show . fromProcessorStrategy

instance PP.Pretty a => PP.Pretty (ProcessorStrategy a) where
  pretty = PP.pretty . fromProcessorStrategy

instance Processor p => Processor (ProcessorStrategy p) where
  type ProofObject (ProcessorStrategy p) = ProofObject p
  type Forking (ProcessorStrategy p)     = Forking p
  type Problem (ProcessorStrategy p)     = Problem p
  name        = name . fromProcessorStrategy
  solve p prob = do
    r1 <- solve (fromProcessorStrategy p) prob
    return $ case r1 of
      Success probs po certfn -> Success probs po certfn
      Fail po                 -> Fail po

instance ParsableProcessor p => ParsableProcessor (ProcessorStrategy p) where
  args p           = args (fromProcessorStrategy p)
  parseProcessor p = parseProcessor (fromProcessorStrategy p)

-- | Generalises 'Strategy' and 'ProcessorStrategy'.
-- Below defined strategy combinators take instances of 'Strategic' as arguments. Hence they work with 'Strategy' and
-- 'ProcessorStrategy'.
class Strategic a where
  toStrategy :: a -> Strategy (Problem a)

instance Strategic (Strategy prob) where
  toStrategy a = a

instance Processor p => Strategic (ProcessorStrategy p) where
  toStrategy = Proc


-- Strategy Combinators ------------------------------------------------------------------------------------------------

-- | Lifts a processor to a 'Strategy'.
strat :: Processor p  => p ->  Strategy (Problem p)
strat = Proc

-- | Lifts a processor to a 'ProcessorStrategy'
pstrat :: Processor p => p -> ProcessorStrategy p
pstrat = ProcessorStrategy

liftS2 :: (Strategic s1, Strategic s2) => (Strategy (Problem s1) -> Strategy (Problem s2) -> t) -> s1 -> s2 -> t
liftS2 f s1 s2 = toStrategy s1 `f` toStrategy s2

infixr 5 >>>, >||> , >=>
infixr 6 <>, <||>

-- | Infix version of 'Then'.
-- @s1 '>>>' s2@ applies @s1@ before @s2@.
-- Fails if @s1@ or @s2@ fails.
--
-- prop> s1 >>> (s2 >>> s3) = (s1 >>> s2) >>> s3 = s1 >>> s2 >>> s3
(>>>) :: (Strategic s1, Strategic s2, Problem s1 ~ Problem s2) => s1 -> s2 -> Strategy (Problem s2)
(>>>)  = liftS2 Then

-- | Infix version of 'ThenPar'.
-- Like ('>>>') but applies @s2@ on all problems in parallel.
(>||>) :: (Strategic s1, Strategic s2, Problem s1 ~ Problem s2) => s1 -> s2 -> Strategy (Problem s2)
(>||>) = liftS2 ThenPar

-- | Like ('>>>') but first strategy is optional.
--
-- prop> s1 >=> s2 = try s1 >>> s2
(>=>) :: (Strategic s1, Strategic s2, Problem s1 ~ Problem s2) => s1 -> s2 -> Strategy (Problem s2)
(>=>) s1 s2  = try s1 >>> s2

-- | Infix version of 'Alt'.
-- @s1 '<>' s2@
--
-- * returns the result of @s1@ if @s1@ is progressing,
-- * returns the result of @s1@ if @s1@ is continuing and @s2@ is failing
-- * returns the result of @s2@ otherwise.
-- * Fails if @s1@ and @s2@ fails.
--
-- prop> s1 <> (s2 <> s3) = (s1 <> s2) <> s3 = s1 <> s2 <> s3
(<>) :: (Strategic s1, Strategic s2, Problem s1 ~ Problem s2) => s1 -> s2 -> Strategy (Problem s2)
(<>) = liftS2 Alt

-- | Infix version of 'OrFaster'.
-- Behaves like ('<>') but applies the strategies in parallel.
-- Suppose that @s2@ ends before @s1@ then:
--
-- prop> s1 <||> s2  = s2 <> s1
(<||>) :: (Strategic s1, Strategic s2, Problem s1 ~ Problem s2) => s1 -> s2 -> Strategy (Problem s2)
(<||>) = liftS2 OrFaster

-- | Timed version of 'OrBetter'.
--
-- @('<?>') cmp s1 s2@ applies @'timeoutIn' n s1@ and @'timeoutIn' n s2@ in parallel and waits until both strategies have
-- finished. Here @n@ depends on 'remainingTime'. We consider the following cases:
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
(<?>) :: (Strategic s1, Strategic s2, Problem s1 ~ prob, Problem s2 ~ prob, ProofData prob) => (ProofTree prob -> ProofTree prob -> Ordering) -> s1 -> s2 -> Strategy prob
(<?>) cmp s1 s2 = OrBetter cmp (strat . to $ toStrategy s1) (strat . to $ toStrategy s2)
  where  to = TimeoutProc Nothing Nothing


trying :: Bool -> Strategy prob -> Strategy prob
trying b s@(Trying _ _) = Trying b s
trying _ (WithStatus f) = WithStatus (try . f)
trying b s              = Trying b s

-- | @'try' s@ is continuing even if @s@ is not.
try :: Strategic s1 => s1 -> Strategy (Problem s1)
try = trying True . toStrategy

-- | @'force' s@ fails if @s@ is not progressing.
force :: Strategic s1 => s1 -> Strategy (Problem s1)
force = trying False . toStrategy

-- | Applied strategy depends on run time status.
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
best :: ProofData prob => (ProofTree prob -> ProofTree prob -> Ordering) -> NList (Strategy prob) -> Strategy prob
best cmp (s:|ss) = foldr1 (cmp <?>) (s:ss)


-- | @'exhaustively' s@ repeatedly applies @s@ until @s@ fails.
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
  type Problem (FailProcessor prob)     = prob
  name _    = "FailProcessor"
  solve _ _ = return $ Fail FailProof

instance ProofData prob => ParsableProcessor (FailProcessor prob) where

-- | Default 'FailProcessor' instance.
failProcessor :: FailProcessor prob
failProcessor = FailProc

-- | @'abort'@ always returns 'Fail' and therfore 'Abort'.
-- Does not abort the whole evaluation in combination with 'try'.
abort :: FailProcessor prob
abort = FailProc


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
      toNat n = case n of
        Just i | i >= 0 -> Just i
        _               -> Nothing
      to = case (toNat $ inT proc, toNat $ untilT proc) of
        (Nothing, Just u ) -> max 0 (u - running)
        (Just i , Nothing) -> i
        (Just i , Just u ) -> min i (max 0 (u - running))
        _                  -> -1
    remains <- (fromMaybe to . toNat . remainingTime) `fmap` askStatus prob
    mr <- timeout (min to remains) (solve (procT proc) prob)
    return $ case mr of
      Nothing       -> Fail (Timeout to)
      Just (Fail p) -> Fail (NoTimeout p)
      Just r@(Success {}) -> Success
        { subProblems   = subProblems r
        , proofData     = NoTimeout (proofData r)
        , certificateFn = certificateFn r }

toSeconds :: Int -> Int
toSeconds n = 1000000*n

instance Processor p => ParsableProcessor (TimeoutProcessor p) where
  args _ ps = argsParser pargs desc
    where
      pargs = TimeoutProc
        <$> optional (fmap toSeconds $ option $ eopt
            `withArgLong` "untilT"
            `withMetavar` "iSec"
            `withHelpDoc` PP.paragraph "Aborts the computation after 'iSec' from the startint time.")
        <*> optional (fmap toSeconds $ option $ eopt
            `withArgLong` "inT"
            `withMetavar` "iSec"
            `withHelpDoc` PP.paragraph "Aborts the computation after 'iSec' from starting the sub processor.")
        <*> argument  (parseSomeParsableProcessorMaybe ps) (eopt
            `withMetavar` "proc"
            `withHelpDoc` PP.string "The applied subprocessor.")
      desc = PP.string "the timeoutprocessor"

-- | Default 'TimeoutProcessor' instance.
timeoutProcessor :: TimeoutProcessor (FailProcessor prob)
timeoutProcessor = TimeoutProc Nothing Nothing failProcessor

-- | @'timoutIn' i p@ aborts the application of @p@ after @min i 'remainingTime'@ seconds;
-- If @i@ is negative the processor may run forever.
timeoutIn :: Processor p => Int -> p -> ProcessorStrategy (TimeoutProcessor p)
timeoutIn n = pstrat . TimeoutProc (Just $ toSeconds n) Nothing

-- | @'timeoutUntil' i p@ aborts the application of @p@ until i seconds wrt. 
-- to the starting time, or if 'remainingTime' is expired.
-- If @i@ is negative the processor may run forever.
timeoutUntil :: Processor p => Int -> p -> ProcessorStrategy (TimeoutProcessor p)
timeoutUntil n = pstrat . TimeoutProc Nothing (Just $ toSeconds n)

