module Tct.Core.Main.Mode
  (
  TctMode (..)
  , defaultMode
  ) where


import Tct.Core.Common.Error
import Tct.Core.Data
import Tct.Core.Main.Options
import Tct.Core.Processor.Failing (failing)


-- | 'TctMode' provides all infromation necesary to construct a Tct instance customised for a problem type.
data TctMode i o opt = TctMode
  { modeId              :: String                                -- ^ id used for package name
  , modeParser          :: String -> Either TctError i           -- ^ The parser for the problem.
  , modeStrategies      :: [StrategyDeclaration i o]             -- ^ Problem specific parsable Processor/Strategies.
                                                                 --   These are added to default 'processors'.
  , modeDefaultStrategy :: Strategy i o                          -- ^ The default strategy to execute.
  , modeOptions         :: Options opt                           -- ^ Problem specific option parser.
                                                                 --   These are added to the standard Tct options.
  , modeModifyer        :: opt -> i -> i                         -- ^ This function is applied to the initial problem,
  , modeAnswer          :: opt -> Return (ProofTree o) -> IO ()  -- ^ Custom Answer.
  }

-- | A default mode. Minimum requirement @modId@ and @modeParser@.
defaultMode :: (ProofData i, ProofData o) => String -> (String -> Either TctError i) -> TctMode i o ()
defaultMode mid mparser = TctMode
  { modeId     = mid
  , modeParser = mparser

  , modeStrategies      = []
  , modeDefaultStrategy = failing
  , modeOptions         = unit
  , modeModifyer        = flip const
  , modeAnswer          = \_ _ -> return () }

