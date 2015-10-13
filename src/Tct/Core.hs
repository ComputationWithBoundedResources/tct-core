{- | This module re-exports top-level definition for

    * configuring Tct,
    * instantiating TcT, and
    * declaring customised Strategies.

  TODO: description

-}
module Tct.Core
  (
  -- * Configuring Tct
  TctConfig (..)
  , defaultTctConfig
  , AnswerFormat (..)
  , ProofFormat (..)
  -- * Initialising Tct
  , addStrategies
  , withDefaultStrategy
  , appendGHCiScript
  , addRuntimeOption
  -- * Processor / Strategy
  , ProofData
  , abortWith
  , succeedWith
  , succeedWith0
  , succeedWith1
  , succeedWithId
  -- ** Argument
  , Argument
  , ArgFlag (..)
  , arg
  , nat
  , bool
  , strat
  , string
  , some
  , withDomain
  , optional
  -- ** Declaration
  , StrategyDeclaration (..)
  , OneTuple (..)
  -- ** Argument and Declaration Modifyer
  , withName
  , withHelp
  -- ** Combinators
  , module Tct.Core.Data.Strategy
  ) where


import Tct.Core.Data
import Tct.Core.Main
import Tct.Core.Data.Strategy

-- | Adds a list of 'StrategyDeclaraton' to the existing ones.
addStrategies :: TctConfig i -> [StrategyDeclaration i i] -> TctConfig i
addStrategies cfg sds = cfg { strategies = (strategies cfg) ++ sds }

-- | Sets the default Strategy.
withDefaultStrategy :: TctConfig i -> Strategy i i -> TctConfig i
withDefaultStrategy cfg st = cfg { defaultStrategy = st }

-- | Sets 'GHCiScript'; and appends the given script.
appendGHCiScript :: TctConfig i -> [String] -> TctConfig i
appendGHCiScript cfg ss = cfg { interactiveGHCi = k (interactiveGHCi cfg) ss}
  where
    k (GHCiCommand _) xs = GHCiScript xs
    k (GHCiScript s1) xs = GHCiScript (s1 ++ xs)

-- | Adds a key-value pair to the runtime options.
addRuntimeOption :: TctConfig i -> String -> [String] -> TctConfig i
addRuntimeOption cfg s ss = cfg { runtimeOptions = (s,ss) :runtimeOptions cfg }

