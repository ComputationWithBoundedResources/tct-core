-- | This module provides common 'Strategy' and 'Processor' combinators.
module Tct.Core.Combinators
  (
  -- * Strategy Combinators
  -- | We define right-associative infix versions of 'Strategy' constructors.
  -- Following precedence holds: optionally,stateful > alternative > sequential > transformations.
  -- For example,
  --
  -- prop> try s1 >>> s2 >||> s3 <> s4 >>> s5 = (try s1) >>> (s2 >||> ((s3 <> s4) >>> s5))
  --
  -- We say that a @'Strategy' s@
  --
  --  * is continuing if the evaluation returns @'Continue' pt@,
  --  * is progressing if additionally @'progress' pt = 'True'@,
  --  * is aborting if the evaluation returns @'Abort' pt@
  --  * is halting otherwise.
  module M
  -- ** Sequential
  , (>>>), (>||>)
  , chain
  , chainWith
  -- ** Transformation
  , (>=>)
  -- ** Alternative
  , (<|>), (<||>), (<?>)
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
  , exhaustively
  , exhaustivelyN
  , es
  , te
  , when
  , check
  , iteProgress
  ) where


import Tct.Core.Data

import Tct.Core.Processor.Succeeding

import Tct.Core.Processor.Annotated     as M
import Tct.Core.Processor.Failing       as M
import Tct.Core.Processor.Identity       as M
import Tct.Core.Processor.Timeout       as M
import Tct.Core.Processor.Transform     as M
import Tct.Core.Processor.TransformWith as M
import Tct.Core.Processor.Wait          as M


-- Strategy Combinators ----------------------------------------------------------------------------------------------

infixr 4 >=>
infixr 5 >>>, >||>
infixr 6 <|>, <||>

-- | Infix version of 'Then'.
-- @s1 '>>>' s2@ applies @s1@ before @s2@.
(>>>) :: Strategy i i -> Strategy i i -> Strategy i i
(>>>)  = Then

-- | Infix version of 'ThenPar'.
-- Like ('>>>') but applies @s2@ on all problems in parallel.
(>||>) :: Strategy i i -> Strategy i i -> Strategy i i
(>||>) = ThenPar

-- | Infix version of 'Trans'.
(>=>) :: Strategy i p -> Strategy p o -> Strategy i o
(>=>) = Trans


-- | Infix version of 'Alt'.
-- @s1 '<|>' s2@
(<|>) :: Strategy i o -> Strategy i o-> Strategy i o
(<|>) = Alt

-- | Infix version of 'OrFaster'.
-- Behaves like ('<|>') but applies the strategies in parallel.
-- Suppose that @s2@ ends before @s1@ then:
--
-- prop> s1 <||> s2  = s2 <> s1
(<||>) :: Strategy i o -> Strategy i o -> Strategy i o
(<||>) = OrFaster

-- | Timed version of 'OrBetter'.
--
-- @('<?>') cmp s1 s2@ applies @'timeoutIn' n s1@ and @'timeoutIn' n s2@ in parallel and waits until both strategies have
-- finished. Here @n@ depends on 'remainingTime'.
-- An example implementation of cmp is:
--
-- > cmp pt1 pt2 = compare (timeUB $ certificate pt1) (timeUB $ certificate pt2)
(<?>) :: ProofData i => (ProofTree o -> ProofTree o -> Ordering) -> Strategy i o -> Strategy i o -> Strategy i o
(<?>) cmp s1 s2 = OrBetter cmp (to s1) (to s2)
  where  to = timeoutRemaining


trying :: Bool -> Strategy i i -> Strategy i i
trying b s@(Trying _ _) = Trying b s
trying _ (WithStatus f) = WithStatus (try . f)
trying b s              = Trying b s

-- | @'try' s@ is continuing even if @s@ is not.
try :: Strategy i i -> Strategy i i
try = trying True

-- | @'force' s@ fails if @s@ is not progressing.
force :: Strategy i i -> Strategy i i
force = trying False

-- | Applied strategy depends on run time status.
withState :: (TctStatus i -> Strategy i o) -> Strategy i o
withState = WithStatus

-- | Specialised version of 'withState'.
withProblem :: (i -> Strategy i o) -> Strategy i o
withProblem g = WithStatus (g . currentProblem)


-- | List version of ('>>>').
--
-- prop> chain [] = identity
chain :: ProofData i => [Strategy i i] -> Strategy i i
chain [] = identity
chain ss = foldr1 (>>>) ss

-- | Like 'chain' but additionally executes the provided strategy after each strategy of the list.
--
-- > chainWith [] (try empty)      == try empty
-- > chainWith [s1,s2] (try empty) == s1 >>> try empty >>> s2 >>> try empty
chainWith :: ProofData i => Strategy i i -> [Strategy i i] -> Strategy i i
chainWith s [] = s
chainWith s ss = foldr1 (\t ts -> t >>> s >>> ts) ss >>> s

-- | List version of ('<|>').
--
-- prop> alternative [] = failing
alternative :: (ProofData i, Show o) => [Strategy i o] -> Strategy i o
alternative [] = failing
alternative ss = foldr1 (<|>) ss

-- | List version of ('<||>').
fastest :: (ProofData i, Show o) => [Strategy i o] -> Strategy i o
fastest [] = failing
fastest ss = foldr1 (<||>) ss

-- | Like 'fastest'. But only runs @n@ strategies in parallel.
fastestN :: (ProofData i, Show o) => Int -> [Strategy i o] -> Strategy i o
fastestN _ [] = failing
fastestN n ss = fastest ss1 <|> fastestN n ss2
  where (ss1,ss2) = splitAt n ss

-- | List version of ('<?>').
best :: (ProofData i, Show o) => (ProofTree o -> ProofTree o -> Ordering) -> [Strategy i o] -> Strategy i o
best _   [] = failing
best cmp ss = foldr1 (cmp <?>) ss

-- | Compares time upperbounds. Useful with 'best'.
cmpTimeUB :: ProofTree i -> ProofTree i -> Ordering
cmpTimeUB pt1 pt2 = compare (tu pt1) (tu pt2)
  where tu = timeUB . certificate


-- | @'exhaustively' s@ repeatedly applies @s@ until @s@ fails.
-- Fails if the first application of @s@ fails.
exhaustively :: Strategy i i -> Strategy i i
exhaustively s =  s >>> try (exhaustively s)

-- | Like 'exhaustively'. But maximal @n@ times.
exhaustivelyN :: ProofData i => Int -> Strategy i i -> Strategy i i
exhaustivelyN n s
  | n > 1     = s >>> try (exhaustivelyN (n-1) s)
  | n == 1    = s
  | otherwise = identity

-- | Short for 'exhaustively'.
es :: Strategy i i -> Strategy i i
es = exhaustively

-- | prop> te st = try (exhaustively st)
te :: Strategy i i -> Strategy i i
te = try . exhaustively

-- | @'when' b st@ applies @st@ if @b@ is true.
when :: ProofData i => Bool -> Strategy i i -> Strategy i i
when b st = if b then st else identity
--whenNot = try $ force s <> sthen

-- | @'check' test msg@. Applies @test@ to the problem.
-- Fails with @msg@ if the test fails. Succeeds otherwise.
check :: ProofData i => (i -> Bool) -> String -> Strategy i i
check f msg = withProblem $ \p -> if f p then succeeding else failing' msg

-- | prop> iteProgress test s1 s2 == test >>> s1, if we have a progress after applying test
-- | prop> iteProgress test s1 s2 == s1, otherwise
iteProgress :: Strategy i i -> Strategy i i -> Strategy i i -> Strategy i i
iteProgress b s1 s2 = force (try b) >>> s1 <|> s2

