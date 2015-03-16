-- | This module provides common 'Strategy' and 'Processor' combinators.
module Tct.Core.Combinators
  (
  -- * Strategy Combinators
  -- | We define left-associative infix versions of 'Strategy' constructors.
  -- Following precedence holds: optionally,stateful > alternative > sequential.
  -- For example,
  -- TODO: left or right + update
  --
  -- prop> try s1 >>> s2 >||> s3 <> s4 >>> s5 = (try s1) >>> (s2 >||> ((s3 <> s4) >>> s5))
  --
  -- We say that a @'Strategy' s@
  --
  --  * is continuing if the evaluation returns @'Continue' pt@,
  --  * is progressing if additionally @'progress' pt = 'True'@,
  --  * is failing if it is not continuing.
  declarations
  -- ** Sequential
  , (>>>), (>||>)
  , chain
  , chainWith
  -- ** Alternative
  , (<>), (<||>), (<?>)
  , alternative
  , fastest
  , fastestN
  , best
  , cmpTimeUB
  -- ** Optional
  , try, force
  -- ** Stateful
  , withState
  , withProblem

  -- ** Combinators
  , when
  , exhaustively
  , exhaustivelyN
  , te
  ) where


import Tct.Core.Data
import Tct.Core.Processor.Trivial
import Tct.Core.Processor.Timeout (timeoutDeclaration, timeoutRemaining)

declarations :: ProofData prob  => [StrategyDeclaration prob]
declarations =
  [ SD failingWithDeclaration
  , SD timeoutDeclaration
  ]

-- FIXME:
-- alternative left or right biased
-- associativy of infix operators; associativity of list versions


-- Strategy Combinators ----------------------------------------------------------------------------------------------

infixl 5 >>>, >||>
infixl 6 <>, <||>

-- | Infix version of 'Then'.
-- @s1 '>>>' s2@ applies @s1@ before @s2@.
-- Fails if @s1@ or @s2@ fails.
--
-- prop> s1 >>> (s2 >>> s3) = (s1 >>> s2) >>> s3 = s1 >>> s2 >>> s3
(>>>) :: Strategy prob -> Strategy prob -> Strategy prob
(>>>)  = Then

-- | Infix version of 'ThenPar'.
-- Like ('>>>') but applies @s2@ on all problems in parallel.
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
(<>) :: Strategy prob -> Strategy prob-> Strategy prob
(<>) = Alt

-- | Infix version of 'OrFaster'.
-- Behaves like ('<>') but applies the strategies in parallel.
-- Suppose that @s2@ ends before @s1@ then:
--
-- prop> s1 <||> s2  = s2 <> s1
(<||>) :: Strategy prob -> Strategy prob -> Strategy prob
(<||>) = OrFaster

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
(<?>) :: ProofData prob => (ProofTree prob -> ProofTree prob -> Ordering)
         -> Strategy prob -> Strategy prob -> Strategy prob
(<?>) cmp s1 s2 = OrBetter cmp (to s1) (to s2)
  where  to = timeoutRemaining


trying :: Bool -> Strategy prob -> Strategy prob
trying b s@(Trying _ _) = Trying b s
trying _ (WithStatus f) = WithStatus (try . f)
trying b s              = Trying b s

-- | @'try' s@ is continuing even if @s@ is not.
try :: Strategy prob -> Strategy prob
try = trying True

-- | @'force' s@ fails if @s@ is not progressing.
force :: Strategy prob -> Strategy prob
force = trying False

-- | Applied strategy depends on run time status.
withState :: (TctStatus prob -> Strategy prob) -> Strategy prob
withState = WithStatus

-- | Specialised version of 'withState'.
withProblem :: (prob -> Strategy prob) -> Strategy prob
withProblem g = WithStatus (g . currentProblem)


emptyList :: ProofData prob => Strategy prob
emptyList = identity

-- | List version of ('>>>').
--
-- prop> chain [] = failWith "empty list"
chain :: ProofData prob => [Strategy prob] -> Strategy prob
chain [] = emptyList
chain ss = foldr1 (>>>) ss

-- | Like 'chain' but additionally executes the provided strategy after each strategy of the list.
--
-- > chainWith [] (try empty)      == try empty
-- > chainWith [s1,s2] (try empty) == s1 >>> try empty >>> s2 >>> try empty
chainWith :: ProofData prob => Strategy prob -> [Strategy prob] -> Strategy prob
chainWith s [] = s
chainWith s ss = foldr1 (\t ts -> t >>> s >>> ts) ss >>> s

-- | List version of ('<>').
alternative :: ProofData prob => [Strategy prob] -> Strategy prob
alternative [] = emptyList
alternative ss = foldr1 (<>) ss

-- | List version of ('<||>').
fastest :: ProofData prob => [Strategy prob] -> Strategy prob
fastest [] = emptyList
fastest ss = foldr1 (<||>) ss

-- | Like 'fastest'. But only runs @n@ strategies in parallel.
fastestN :: ProofData prob => Int -> [Strategy prob] -> Strategy prob
fastestN _ [] = identity
fastestN n ss = fastest ss1 <> fastestN n ss2
  where (ss1,ss2) = splitAt n ss

-- | List version of ('<?>').
best :: ProofData prob => (ProofTree prob -> ProofTree prob -> Ordering) -> [Strategy prob] -> Strategy prob
best _   [] = emptyList
best cmp ss = foldr1 (cmp <?>) ss

-- | Compares time upperbounds. Useful with 'best'.
cmpTimeUB :: ProofTree prob -> ProofTree prob -> Ordering
cmpTimeUB pt1 pt2 = compare (tu pt1) (tu pt2)
  where tu = timeUB . certificate


-- | @'exhaustively' s@ repeatedly applies @s@ until @s@ fails.
-- Fails if the first application of @s@ fails.
exhaustively :: Strategy prob -> Strategy prob
exhaustively s =  s >>> try (exhaustively s)

-- | Like 'exhaustively'. But maximal @n@ times.
exhaustivelyN :: ProofData prob => Int -> Strategy prob -> Strategy prob
exhaustivelyN n s
  | n<0       = identity
  | otherwise = s >>> try (exhaustivelyN (n-1) s)

-- | prop> te st = try (exhaustively st)
te :: Strategy prob -> Strategy prob
te = try . exhaustively

-- | @'when' b st@ applies @st@ if @b@ is true.
when :: ProofData prob => Bool ->  Strategy prob -> Strategy prob
when b st = if b then st else identity
--whenNot = try $ force s <> sthen

