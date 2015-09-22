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
  -- * Processor / Strategy
  , toStrategy
  , ProofData
  , Strategy
  -- ** Argument
  , Argument
  , ArgFlag (..)
  , nat
  , bool
  , strat
  , some
  , optional
  -- ** Declaration
  , StrategyDeclaration (..)
  , strategy
  , OneTuple (..)
  -- ** Argument and Declaration Modifyer
  , withName
  , withHelp
  -- ** Combinators
  , module Tct.Core.Combinators
  ) where


import Tct.Core.Combinators
import Tct.Core.Data
import Tct.Core.Main


-- | Adds a list of 'StrategyDeclaraton' to the existing ones.
addStrategies :: TctConfig i -> [StrategyDeclaration i i] -> TctConfig i
addStrategies cfg sds = cfg { strategies = sds }

-- | Sets the default Strategy.
withDefaultStrategy :: TctConfig i -> Strategy i i -> TctConfig i
withDefaultStrategy cfg st = cfg { defaultStrategy = st }

