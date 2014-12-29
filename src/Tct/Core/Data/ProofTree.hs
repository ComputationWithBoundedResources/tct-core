-- | This module provides the 'ProofTree' type.
-- 'ProofTree' stores the open problems and the proofs of applying 'Processor' instances to problems.
module Tct.Core.Data.ProofTree
  (
  -- * ProofTree
  ProofNode (..)
  , ProofTree (..)
  -- * Certification
  , collectCertificate
  , certificate
  -- * Properites
  , progress
  , open
  , isOpen
  , isClosed
  ) where


import           Control.Applicative       as A ((<$>))
import           Data.Foldable             as F (Foldable, foldMap, foldr, toList)
import           Data.Traversable          as T (Traversable, traverse)

import qualified Tct.Core.Common.Pretty    as PP
import           Tct.Core.Data.Certificate (Certificate, unbounded)
import           Tct.Core.Data.Types


-- | Computes the 'Certificate' of 'ProofTree'.
collectCertificate :: ProofTree Certificate -> Certificate
collectCertificate (Open c)                      = c
collectCertificate (NoProgress _ subtree)        = collectCertificate subtree
collectCertificate (Progress _ certfn' subtrees) = certfn' (collectCertificate `fmap` subtrees)

-- | Computes the 'Certificate' of a 'ProofTree'.
-- 'Open' nodes have the 'Certificate' 'unboundend'.
--
-- prop> certificate pt = collectCertificate (const unbounded `fmap` pt)
certificate :: ProofTree l -> Certificate
certificate pt = collectCertificate $ const unbounded `fmap` pt


-- | Checks if the 'ProofTree' contains a 'Progress' node.
progress :: ProofTree l -> Bool
progress (Open _)          = False
progress (NoProgress _ pt) = progress pt
progress (Progress {})     = True

-- | Returns the 'Open' nodes of a 'ProofTree'.
open :: ProofTree l -> [l]
open = F.foldr (:) []

-- | Checks if there exists 'Open' nodes in the 'ProofTree'.
isOpen :: ProofTree l -> Bool
isOpen = not . isClosed

-- | Checks if there are no 'Open' nodes in the 'ProofTree'.
--
-- prop> isClosed = not . isOpen
isClosed :: ProofTree l -> Bool
isClosed = null . open


instance Functor ProofTree where
  f `fmap` Open l              = Open (f l)
  f `fmap` NoProgress pn pt    = NoProgress pn (f `fmap` pt)
  f `fmap` Progress pn cns pts = Progress pn cns ((f `fmap`) `fmap` pts)

instance Foldable ProofTree where
  f `foldMap` (Open l)           = f l
  f `foldMap` (NoProgress _ pt)  = f `foldMap` pt
  f `foldMap` (Progress _ _ pts) = (f `foldMap`) `foldMap` pts

instance Traversable ProofTree where
  f `traverse` (Open l)              = Open A.<$> f l
  f `traverse` (NoProgress pn pt)    = NoProgress pn A.<$> f `traverse` pt
  f `traverse` (Progress pn cfn pts) = Progress pn cfn A.<$> (f `traverse`) `traverse` pts


instance Show (ProofTree l) where
  show _ = undefined


-- Pretty Printing ---------------------------------------------------------------------------------------------------

-- TODO: different modes; compact ...

instance Processor p => PP.Pretty (ProofNode p) where
  pretty (ProofNode p prob po) = PP.vcat
    [ PP.text "Considered Problem:" PP.<$$> ind (PP.pretty prob)
    , PP.text "Applied Processor:"  PP.<$$> ind (PP.text $ show p)
    , PP.text "Proof:"              PP.<$$> ind (PP.pretty po) ]
    where ind = PP.indent 2

instance PP.Pretty l => PP.Pretty (ProofTree l) where
  pretty (Open l) = PP.vcat
    [ PP.text "*** Open ***"
    , PP.indent 4 (PP.pretty l) ]
  pretty (NoProgress _ pt) = PP.pretty pt
  {-pretty (NoProgress pn pt) = PP.vcat-}
    {-[ PP.text "*** NoProgress ***"-}
    {-, PP.indent 4 (PP.pretty pn)-}
    {-, PP.indent 2 (PP.pretty pt) ]-}
  pretty (Progress pn _ pts) = PP.vcat
    [ PP.text "*** Progress ***"
    , PP.indent 4 (PP.pretty pn)
    , PP.indent 2 (PP.vcat $ map PP.pretty (F.toList pts)) ]

