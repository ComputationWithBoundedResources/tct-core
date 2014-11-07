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
  -- * Initialising Tct
  , TctMode
  , withStrategies
  , withDefaultStrategy
  , apply
  , applyMode
  -- * Strategy Declaration
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
withStrategies :: TctMode prob opt -> [StrategyDeclaration prob] -> TctMode prob opt
withStrategies m sds = m { modeStrategies = modeStrategies m ++ sds }

-- | Sets the default Strategy.
withDefaultStrategy :: TctMode prob opt -> Strategy prob -> TctMode prob opt
withDefaultStrategy m st = m { modeDefaultStrategy = st }

