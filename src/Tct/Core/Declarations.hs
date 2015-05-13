
module Tct.Core.Declarations (declarations) where

import Tct.Core.Parse ()
import Tct.Core.Combinators
import Tct.Core.Data


declarations :: ProofData i => [StrategyDeclaration i i]
declarations =
  [ SD identityDeclaration
  , SD failingDeclaration
  , SD timeoutInDeclaration
  , SD waitDeclaration
  ]
