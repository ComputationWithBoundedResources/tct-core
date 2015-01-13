module Tct.Core.Main.Mode
  (
  TctMode (..)
  ) where


import Tct.Core.Common.Error
import Tct.Core.Data
import Tct.Core.Main.Options


-- | 'TctMode' provides all infromation necesary to construct a Tct instance customised for a problem type.
data TctMode prob opt = TctMode
  { modeId              :: String                                -- ^ id used for package name
  , modeParser          :: String -> Either TctError prob        -- ^ The parser for the problem.
  , modeStrategies      :: [StrategyDeclaration prob]            -- ^ Problem specific parsable Processor/Strategies.
                                                                 --   These are added to default 'processors'.
  , modeDefaultStrategy :: Strategy prob                         -- ^ The default strategy to execute.
  , modeOptions         :: Options opt                           -- ^ Problem specific option parser.
                                                                 --   These are added to the standard Tct options.
  , modeModifyer        :: prob -> opt -> prob                   -- ^ This function is applied to the initial problem,
                                                                 --   using the options parsed from command line.
  , modeAnswer          :: ProofTree prob -> IO ()               -- ^ Custom Answer.
  }

