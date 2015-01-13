{-# OPTIONS_HADDOCK not-home #-}
-- | This module defines the most important types.
module Tct.Core.Data.Types where


import           Control.Applicative           (Applicative)
import           Control.Monad.Error           (MonadError)
import           Control.Monad.Reader          (MonadIO, MonadReader, ReaderT)
import           Data.Dynamic                  (Dynamic)
import           Data.Foldable                 as F (Foldable)
import           Data.Traversable              as T (Traversable)
import qualified System.Time                   as Time
import           Text.Parsec                   (alphaNum, letter, oneOf, (<|>))
import qualified Text.Parsec.Language          as PL
import qualified Text.Parsec.Token             as PT
import           Text.ParserCombinators.Parsec (CharParser)

import qualified Tct.Core.Common.Pretty        as PP
import qualified Tct.Core.Common.Xml           as Xml
import qualified Tct.Core.Data.Certificate     as C
import           Tct.Core.Data.Forks           (Id (..))


-- TcT Monad ---------------------------------------------------------------------------------------------------------

-- | Provides Tct runtime options.
data TctROState = TctROState
  { startTime    :: Time.ClockTime
  , stopTime     :: Maybe Time.ClockTime }

-- | The Tct monad.
newtype TctM r = TctM { runTct :: ReaderT TctROState IO r}
    deriving (Monad, Applicative, MonadIO, MonadReader TctROState, Functor, MonadError IOError)

-- | Defines the (dynamic) runtime status of 'TctROState'.
data TctStatus prob = TctStatus
  { currentProblem :: prob
  , runningTime    :: Int       -- ^ Runing time in seconds.
  , remainingTime  :: Maybe Int -- ^ Remaining time in seconds.
  }


-- Proof Trees -------------------------------------------------------------------------------------------------------

-- | A 'ProofNode' stores the necessary information to construct a (formal) proof from the application of a 'Processor'.
data ProofNode p = ProofNode
  { processor :: p
  , problem   :: Problem p
  , proof     :: ProofObject p }

-- | A 'ProofTree' is constructed by applying a 'Tct.Core.Strategy' to a problem.
-- During evaluation
--
-- * 'Open' nodes store the open (sub-)problems,
-- * 'NoProgress' nodes result from failing 'Processor' applications, and
-- * 'Progress' nodes result from successfull 'Processor' application.
data ProofTree l where
  Open       :: l -> ProofTree l
  NoProgress :: Processor p => ProofNode p -> ProofTree l -> ProofTree l
  Progress   :: Processor p => ProofNode p -> CertificateFn p -> Forking p (ProofTree l) -> ProofTree l


-- Processor  --------------------------------------------------------------------------------------------------------

-- | 'Fork' is an abstract type that provides the "Foldable", "Functor" and "Traversable" interface.
type Fork t = (Foldable t, Functor t, Traversable t)

-- | Provides the interface for the proof construction.
-- All types which occur in the proof construction have to implement 'ProofData'.
type ProofData d = (Show d, PP.Pretty d, Xml.Xml d)

-- | Type synonym for functions that defines how a 'C.Certificate' is computed from a collection of @'C.Certificate's@.
type CertificateFn p = Forking p C.Certificate -> C.Certificate

-- | The result of applying a @'Processor' p@ to a problem.
data Result p
  = Fail
    { proofData :: ProofObject p }
  | Success
    { subProblems   :: Forking p (Problem p)
    , proofData     :: ProofObject p
    , certificateFn :: CertificateFn p }

class (Show p, ProofData (ProofObject p), ProofData (Problem p), Fork (Forking p)) => Processor p where
  type ProofObject p   :: *
  type Problem p       :: *
  type Forking p       :: * -> *
  type Forking p       =  Id
  solve                :: p -> Problem p -> TctM (Return (ProofTree (Problem p)))


-- Strategy ----------------------------------------------------------------------------------------------------------

-- | A 'Strategy' composes instances of 'Processor' and specifies in which order they are applied.
-- For a detailed description of the combinators see "Tct.Combinators".
data Strategy prob where
  Proc       :: (Processor p, Problem p ~ prob) => p -> Strategy prob
  Trying     :: Bool -> Strategy prob -> Strategy prob
  Then       :: Strategy prob -> Strategy prob -> Strategy prob
  ThenPar    :: Strategy prob -> Strategy prob -> Strategy prob
  Alt        :: Strategy prob -> Strategy prob -> Strategy prob
  OrFaster   :: Strategy prob -> Strategy prob -> Strategy prob
  OrBetter   :: (ProofTree prob -> ProofTree prob -> Ordering) -> Strategy prob -> Strategy prob -> Strategy prob
  WithStatus :: (TctStatus prob -> Strategy prob) -> Strategy prob

-- | 'Return' specifies if the evaluation of a strategy is aborted or continued.
-- See "Combinators" fndor a detailed description.
data Return l
  = Continue { fromReturn :: l }
  | Abort    { fromReturn :: l }
  deriving (Show, Functor)


-- Answer ------------------------------------------------------------------------------------------------------------

-- | A generic answer type.
data SomeAnswer                 where SomeAnswer :: ProofData a => a -> SomeAnswer
instance Show (SomeAnswer)      where show (SomeAnswer a) = show a
instance PP.Pretty (SomeAnswer) where pretty (SomeAnswer a) = PP.pretty a


-- Heterogenous List -------------------------------------------------------------------------------------------------

-- | A heterogenous list.
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

type family HListOf a :: [*] where
  HListOf ()               = '[]
  HListOf (a1,a2)          = '[a1,a2]
  HListOf (a1,a2,a3)       = '[a1,a2,a3]
  HListOf (a1,a2,a3,a4)    = '[a1,a2,a3,a4]
  HListOf (a1,a2,a3,a4,a5) = '[a1,a2,a3,a4,a5]
  HListOf (OneTuple a)     = '[a]

class ToHList a                   where toHList :: a -> HList (HListOf a)
instance ToHList ()               where toHList ()               = HNil
instance ToHList (a1,a2)          where toHList (a1,a2)          = HCons a1 (HCons a2 HNil)
instance ToHList (a1,a2,a3)       where toHList (a1,a2,a3)       = HCons a1 (toHList (a2,a3))
instance ToHList (a1,a2,a3,a4)    where toHList (a1,a2,a3,a4)    = HCons a1 (toHList (a2,a3,a4))
instance ToHList (a1,a2,a3,a4,a5) where toHList (a1,a2,a3,a4,a5) = HCons a1 (toHList (a2,a3,a4,a5))
instance ToHList (OneTuple a)     where toHList (OneTuple a)     = HCons a HNil

-- | Should be used in 'strategy' ('declareProcessor') if the Strategy (Processor) has a single argument.
newtype OneTuple a = OneTuple a


-- Currying / Uncurrying ---------------------------------------------------------------------------------------------

data as :-> b = HList as :-> b
infix 4 :->

-- | Uncurried version of a function.
type family Uncurry a where
  Uncurry ('[] :-> r) = r
  Uncurry ((a ': as) :-> r) = a -> Uncurry (as :-> r)


-- | Return type of function wrt to its argument list.
type family Ret as f where
  Ret '[] b = b
  Ret (a ': as) (a -> b) = Ret as b


-- Declarations ------------------------------------------------------------------------------------------------------

-- | Specifies if the Argument is optional or required.
-- This mainly affects parsing of strategies and the the default function ('defaultFun') of declarations.
data ArgFlag = Optional | Required

-- | Specifies an meta information of an argument.
-- An argument contains meta information - name, domain and description - for displaying and parsing.
-- An optional argument additionally requires a default value.
data Argument :: ArgFlag -> * -> * where
  ReqArg :: r ~ Required => 
    { argName :: String, argDomain :: String, argHelp :: [String] } -> Argument r a
  OptArg :: r ~ Optional => 
    { argName :: String, argDomain :: String, argHelp :: [String], argDefault :: a } -> Argument r a

-- | Associates the types to a list of arguments.
type family ArgsType a where
  ArgsType (Argument r a ': as) = a ': ArgsType as
  ArgsType '[]                  = '[]

-- | A 'Strategy' or a 'Processor' is wrapped into a declaration 
data Declaration :: * -> * where
  Decl :: (f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f), Ret (ArgsType args) f ~ Strategy prob)
    => String                                       -- The name of the declaration.
    -> [String]                                     -- A description of the declaration.
    -> f                                            -- A uncurried version of the Strategy or Processor.
    -> HList args                                   -- A list of arguments.
    -> Declaration (args :-> Strategy prob)

-- | Specifies the construction of a argument parser.
class ParsableArgs prob ats where
  mkOptParsers :: HList ats -> [SParser prob (String,Dynamic)]
  mkArgParser  :: HList ats -> [(String, Dynamic)] -> SParser prob (HList (ArgsType ats))

-- | Collects the meta information of a list of arguments.
class ArgsInfo as where
  argsInfo :: 
    HList as ->                                -- A heterogenous list of arguments.
    [(String, String, [String], Maybe String)] -- A list of (name, domain, description, default value)

-- | Existential type for declarations specifying a Strategy.
data StrategyDeclaration prob where
  SD :: (ParsableArgs prob args, ArgsInfo args) => Declaration (args :-> Strategy prob) -> StrategyDeclaration prob


-- Parsing -----------------------------------------------------------------------------------------------------------

type SPState prob = [StrategyDeclaration prob]
type SParser prob = CharParser (SPState prob)

class SParsable prob a where
  parseS :: SParser prob a

-- | Specified Tokenparser.
strategyTP :: PT.TokenParser st
strategyTP = PT.makeTokenParser style
  where
    style = PL.emptyDef
      { PT.commentStart   = "{-"
      , PT.commentEnd     = "-}"
      , PT.commentLine    = "--"
      , PT.nestedComments = True
      , PT.identStart     = letter
      , PT.identLetter    = alphaNum <|> oneOf "_'"
      , PT.reservedOpNames= ["try", "force"]
      , PT.reservedNames  = [">>>", ">||", "<>", "<||>" ]
      , PT.caseSensitive  = True }


-- Modifyers ---------------------------------------------------------------------------------------------------------

-- | Update of meta information.
class WithName a where  withName :: a -> String -> a

-- | Update of meta information.
class WithHelp a where  withHelp :: a -> [String] -> a

