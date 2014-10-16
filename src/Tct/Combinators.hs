-- | This module provides common 'Strategy' and 'Processor' combinators.
module Tct.Combinators
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
  , NList (..)
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
  ) where


import Data.Data (Typeable)

import Tct.Core
import Tct.Processors.Timeout (timeoutRemaining, timeoutDeclaration)
import Tct.Processors.Failing (failingDeclaration)

declarations :: (ProofData prob, Typeable prob)  => [StrategyDeclaration prob]
declarations = 
  [ SD failingDeclaration
  , SD timeoutDeclaration
  ]


-- Strategy Combinators ----------------------------------------------------------------------------------------------

infixl 5 >>>, >||> , >=>
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

-- | Like ('>>>') but first strategy is optional.
--
-- prop> s1 >=> s2 = try s1 >>> s2
(>=>) :: Strategy prob -> Strategy prob -> Strategy prob
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

