{- | This module re-exports the Tct.Core.* modules.


We provide a short description of the core:

Intuitively,

  * 'ProofTree' corresponds to the global program state,
  * a 'Processor' instance corresponds to an expression modifying the state,
  * 'Strategy' controls the flow,
  * problem @prob@ is the input, and
  * 'Certificate' is a property of the 'ProofTree'.

The type of @prob@ is abstract but identical for the whole evaulation.
The application of a 'Strategy' to a problem @prob@ proceeds as follows:

[Initialisation]
  Given a problem @prob@, the initial state is the 'ProofTree' node @'Open' prob@.
  We say that @prob@ is an open problem.

[Step]
  In each step a @'Processor' p@ is applied to all open problems of the 'ProofTree' (or a subtree of it).
  Which 'Processor' is applied is specified by the applied 'Strategy'.
  The application of @p@ to prob may be a

    ['Success']
        Then @'Open' prob@ is transformed to @'Progress' proof certfn -> {'Open' prob1, ...,, 'Open' probn}@. Here
        @proof@ contains a formal proof of the transformation and @certfn@ is a function that defines how the
        results of the subproblems are combined
        ; or

    ['Fail']
        Then @'Open' prob@ is transformed to @'NoProgress' proof -> 'Open' prob@.

The evaluation results in a final 'ProofTree'.
If there are any open problems left, the overall result is undefined.
Otherwise, all leafs of the 'ProofTree' are 'Progress' nodes providing a 'Certificate' of the considered subproblem.
These are recursively combined (in a bottom-up way) using the certification function of 'Progress' nodes.

-}

module Tct.Core
  (
    module M
  ) where


import Tct.Core.Certificate as M
import Tct.Core.Forks       as M
import Tct.Core.Processor   as M
import Tct.Core.ProofTree   as M
import Tct.Core.Strategy    as M
import Tct.Core.TctM        as M 



