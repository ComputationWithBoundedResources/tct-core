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
  , TctMode
  , defaultMode
  , withStrategies
  , withDefaultStrategy
  , setMode
  , setModeWith
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
withStrategies :: TctMode i o opt -> [StrategyDeclaration i o] -> TctMode i o opt
withStrategies m sds = m { modeStrategies = modeStrategies m ++ sds }

-- | Sets the default Strategy.
withDefaultStrategy :: TctMode i o opt -> Strategy i o -> TctMode i o opt
withDefaultStrategy m st = m { modeDefaultStrategy = st }

