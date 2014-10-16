{- | This module re-exports top-level definition for

    * configuring Tct,
    * instantiating TcT, and
    * declaring customised Strategies.

  TODO: description


-}
module Tct
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
  , module Tct.Combinators
  ) where


import Tct.Combinators
import Tct.Core
import Tct.Main


-- | Adds a list of 'StrategyDeclaraton' to the existing ones.
withStrategies :: TctMode prob opt -> [StrategyDeclaration prob] -> TctMode prob opt
withStrategies m sds = m { modeStrategies = modeStrategies m ++ sds }

-- | Sets the default Strategy.
withDefaultStrategy :: TctMode prob opt -> Strategy prob -> TctMode prob opt
withDefaultStrategy m st = m { modeDefaultStrategy = st }

