-- | This module defines the most important types.
module Tct.Core.Data.Types where


import           Control.Applicative
import           Control.Monad.Error           (MonadError)
import           Control.Monad.Reader          (MonadIO, MonadReader, ReaderT)
import           Data.Dynamic                  (Dynamic)
import           Data.Foldable                 as F (Foldable)
import           Data.Traversable              as T (Traversable)
import           Data.Typeable
import qualified System.Time                   as Time
import           Text.Parsec                   (alphaNum, letter, oneOf)
import qualified Text.Parsec                   as P ((<|>))
import qualified Text.Parsec.Language          as PL
import qualified Text.Parsec.Token             as PT
import           Text.ParserCombinators.Parsec (CharParser)

import qualified Tct.Core.Common.Pretty        as PP
import qualified Tct.Core.Common.Xml           as Xml
import qualified Tct.Core.Data.Certificate     as C
import           Tct.Core.Data.Forks           (Id (..))


--- * TctM Monad -----------------------------------------------------------------------------------------------------

-- | Provides Tct runtime options. The State of TcTM monad.
data TctROState = TctROState
  { startTime     :: Time.ClockTime
  , stopTime      :: Maybe Time.ClockTime
  , tempDirectory :: FilePath
  , solver        :: Maybe (FilePath, [String])
  }


-- | The Tct monad.
newtype TctM r = TctM { runTct :: ReaderT TctROState IO r }
  deriving (Monad, Applicative, MonadIO, MonadReader TctROState, Functor, MonadError IOError)

-- | Defines the (read-only) runtime status of 'TctROState'.
data TctStatus prob = TctStatus
  { currentProblem :: prob      -- ^ Current Problem.
  , runningTime    :: Int       -- ^ Runing time in seconds.
  , remainingTime  :: Maybe Int -- ^ Remaining time in seconds.
  }


--- * Proof Trees ----------------------------------------------------------------------------------------------------

-- | A 'ProofNode' stores the necessary information to construct a (formal) proof from the application of a 'Processor'.
data ProofNode p = ProofNode
  { processor :: p
  , problem   :: I p
  , proof     :: ProofObject p }

-- | A 'ProofTree' is constructed by applying a 'Tct.Core.Strategy' to a problem.
-- During evaluation
--
-- * 'Open' nodes store the open (sub-)problems,
-- * 'NoProgress' nodes result from failing 'Processor' applications ('Fail'), and
-- * 'Progress' nodes result from successfull 'Processor' application ('Success').
--
-- The type of the proof tree depends on the open nodes. A prooftree wihtout open nodes can be casted to any type.
data ProofTree l where
  Open       :: l -> ProofTree l
  NoProgress :: Processor p => ProofNode p -> ProofTree l -> ProofTree l
  Progress   :: Processor p => ProofNode p -> CertificateFn p -> Forking p (ProofTree l) -> ProofTree l


--- * Processor  -----------------------------------------------------------------------------------------------------

-- | 'Fork' is an abstract type that provides the "Foldable", "Functor" and "Traversable" interface.
type Fork t = (Foldable t, Functor t, Traversable t)

-- | Provides the interface for the proof construction.
-- All types which occur in the proof construction have to implement 'ProofData'.
type ProofData d = (Show d, PP.Pretty d, Xml.Xml d)

-- | Type synonym for functions that defines how a 'C.Certificate' is computed from a collection of 'C.Certificate's.
type CertificateFn p = Forking p C.Certificate -> C.Certificate

-- | The result of applying a @'Processor' p@ to a problem.
data Result p
  = Fail
    { proofData :: ProofObject p }
  | Success
    { subProblems   :: Forking p (O p)
    , proofData     :: ProofObject p
    , certificateFn :: CertificateFn p }

-- | 'Return' specifies if the evaluation of a strategy is aborted or continued.
-- See "Combinators" for a detailed description.
--
-- The 'Halt' constructor stores a closed prooftree which is only used for output. As 'Halt' does not depend on any
-- type 'Return' can be casted to any type  in this case.
data Return l where
  Continue :: { fromReturn :: l } -> Return l
  Abort    :: { fromReturn :: l } -> Return l
  Halt     :: ProofTree ProofBox -> Return l

-- | Existential type for ProofData.
data ProofBox where
  ProofBox :: ProofData l => l -> ProofBox

instance PP.Pretty ProofBox where
  pretty (ProofBox p) = PP.pretty p

instance Functor Return where
  f `fmap` Continue l = Continue (f l)
  f `fmap` Abort l    = Abort (f l)
  _ `fmap` Halt pt    = Halt pt

instance Show l => Show (Return l) where
  show (Continue l) = "Continue: " ++ show l
  show (Abort l)    = "Abort " ++ show l
  show (Halt _)     = "Halt "

-- | Everything that is necessary for defining a processor.
class (Show p, ProofData (ProofObject p), ProofData (I p), Fork (Forking p)) => Processor p where
  type ProofObject p :: *                                           -- ^ The type of the proof.
  type I p           :: *                                           -- ^ The type of the input problem.
  type O p           :: *                                           -- ^ The type of the output problem.
  type Forking p     :: * -> *                                      -- ^ The type of the (children) collection.
  solve              :: p -> I p -> TctM (Return (ProofTree (O p)))

  type Forking p     =  Id


-- Strategy ----------------------------------------------------------------------------------------------------------

-- | A 'Strategy' composes instances of 'Processor' and specifies in which order they are applied.
-- For a detailed description of the combinators see "Tct.Combinators".
data Strategy i o where
  Proc       :: (Processor p) => p -> Strategy (I p) (O p)

  -- | Problem type transformation
  Trans      :: ProofData p => Strategy i p -> Strategy p o -> Strategy i o

  -- | Sequentiel
  Then       :: Strategy i i -> Strategy i i -> Strategy i i
  ThenPar    :: Strategy i i -> Strategy i i -> Strategy i i


  -- | Alternative
  Alt        :: Strategy i o -> Strategy i o -> Strategy i o
  OrFaster   :: Strategy i o -> Strategy i o -> Strategy i o
  OrBetter   :: (ProofTree o -> ProofTree o -> Ordering) -> Strategy i o -> Strategy i o -> Strategy i o

  -- | Optional
  Trying     :: Bool -> Strategy i i -> Strategy i i

  -- | Stateful
  WithStatus :: (TctStatus i -> Strategy i o) -> Strategy i o
  WithState  :: (TctROState -> TctROState) -> Strategy i o -> Strategy i o
  deriving Typeable


-- Heterogenous List -------------------------------------------------------------------------------------------------

-- | A heterogenous list.
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

type family HListOf a :: [*] where
  HListOf ()                        = '[]
  HListOf (a1,a2)                   = '[a1,a2]
  HListOf (a1,a2,a3)                = '[a1,a2,a3]
  HListOf (a1,a2,a3,a4)             = '[a1,a2,a3,a4]
  HListOf (a1,a2,a3,a4,a5)          = '[a1,a2,a3,a4,a5]
  HListOf (a1,a2,a3,a4,a5,a6)       = '[a1,a2,a3,a4,a5,a6]
  HListOf (a1,a2,a3,a4,a5,a6,a7)    = '[a1,a2,a3,a4,a5,a6,a7]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8) = '[a1,a2,a3,a4,a5,a6,a7,a8]
  HListOf (OneTuple a)              = '[a]

class ToHList a                            where toHList :: a -> HList (HListOf a)
instance ToHList ()                        where toHList ()                        = HNil
instance ToHList (a1,a2)                   where toHList (a1,a2)                   = HCons a1 (HCons a2 HNil)
instance ToHList (a1,a2,a3)                where toHList (a1,a2,a3)                = HCons a1 (toHList (a2,a3))
instance ToHList (a1,a2,a3,a4)             where toHList (a1,a2,a3,a4)             = HCons a1 (toHList (a2,a3,a4))
instance ToHList (a1,a2,a3,a4,a5)          where toHList (a1,a2,a3,a4,a5)          = HCons a1 (toHList (a2,a3,a4,a5))
instance ToHList (a1,a2,a3,a4,a5,a6)       where toHList (a1,a2,a3,a4,a5,a6)       = HCons a1 (toHList (a2,a3,a4,a5,a6))
instance ToHList (a1,a2,a3,a4,a5,a6,a7)    where toHList (a1,a2,a3,a4,a5,a6,a7)    = HCons a1 (toHList (a2,a3,a4,a5,a6,a7))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8) where toHList (a1,a2,a3,a4,a5,a6,a7,a8) = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8))
instance ToHList (OneTuple a)              where toHList (OneTuple a)              = HCons a HNil

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


--- * Declarations ---------------------------------------------------------------------------------------------------

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

-- | A declaration associates a function with name, description, arguments.
data Declaration :: * -> * where
  Decl :: (f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f)) =>
    String -> [String] -> f -> HList args -> Declaration (args :-> Ret (ArgsType args) f)

declare ::
  (ToHList a, HListOf a ~ args, Uncurry (ArgsType args :-> Ret (ArgsType args) f) ~ f) =>
    String -> [String] -> a -> f -> Declaration (args :-> Ret (ArgsType args) f)
declare n desc as p = Decl n desc p (toHList as)

-- | Specifies the construction of a argument parser.
class ParsableArgs i o ats where
  mkOptParser :: HList ats -> [SParser i o (String,Dynamic)]
  mkArgParser :: HList ats -> [(String, Dynamic)] -> SParser i o (HList (ArgsType ats))

-- | Collects the meta information of a list of arguments.
class ArgsInfo as where
  argsInfo ::
    HList as ->                                -- A heterogenous list of arguments.
    [(String, String, [String], Maybe String)] -- A list of (name, domain, description, default value)

-- | Existential type for declarations specifying a Strategy.
-- Mainly used for parsing and description.
data StrategyDeclaration i o where
  SD :: (ParsableArgs i o args, ArgsInfo args) => Declaration (args :-> Strategy i o) -> StrategyDeclaration i o


--- * Parsing --------------------------------------------------------------------------------------------------------

type SPState i o = [StrategyDeclaration i o]
type SParser i o  = CharParser (SPState i o)

class SParsable i o a where
  parseS :: SParser i o a

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
      , PT.identLetter    = alphaNum P.<|> oneOf "_'"
      , PT.reservedOpNames= ["try", "force"]
      , PT.reservedNames  = [">>>", ">||", "<>", "<||>" ]
      , PT.caseSensitive  = True }


--- * Modifyers ------------------------------------------------------------------------------------------------------

-- | Update of meta information.
class WithName a where  withName :: a -> String -> a

-- | Update of meta information.
class WithHelp a where  withHelp :: a -> [String] -> a

