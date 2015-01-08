module Tct.Core.Main.Mode
  (
  TctMode (..)
  ) where


import Tct.Core.Common.Error
import Tct.Core.Data
import Tct.Core.Main.Options


-- | 'TctMode' provides all infromation necesary to construct a Tct instance customised for a problem type.
data TctMode prob opt = TctMode
  { modeParser          :: String -> Either TctError prob        -- ^ The parser for the problem.
  , modeStrategies      :: [StrategyDeclaration prob]            -- ^ Problem specific parsable Processor/Strategies.
                                                                 --   These are added to default 'processors'.
  , modeDefaultStrategy :: Strategy prob                         -- ^ The default strategy to execute.
  , modeOptions         :: Options opt                           -- ^ Problem specific option parser.
                                                                 --   These are added to the standard Tct options.
  , modeModifyer        :: prob -> opt -> prob                   -- ^ This function is applied to the initial problem,
                                                                 --   using the options parsed from command line.
  , modeAnswer          :: ProofTree prob -> SomeAnswer          -- ^ Custom Answer.
  }


{-data Void = Void deriving (Show, Read)-}
{-instance PP.Pretty Void where pretty = const $ PP.string "Void"-}

{-instance Xml.Xml Void where-}
  {-toXml _ = Xml.elt "void" []-}

-- | An example 'TctMode'.
{-void :: TctMode Void Void-}
{-void = TctMode-}
  {-{ modeParser          = const (Right Void)-}
  {-, modeStrategies      = []-}
  {-, modeDefaultStrategy = failing-}
  {-, modeOptions         = pure Void-}
  {-, modeModifyer        = const id-}
  {-, modeAnswer          = const (answer Void)}-}

