-- | This module provides the 'ProofTree' type.
module Tct.Core.ProofTree
  (
  -- * ProofTree
  ProofNode (..)
  , ProofTree (..)
  -- * Certification
  , certificate
  , collectCertificate
  -- * Lifting
  , resultToTree
  -- * Properites
  , progress
  , open
  , isOpen
  , isClosed
  ) where


import Control.Applicative  as A ((<$>))
import Data.Foldable        as F (Foldable, foldMap, foldr, toList)
import Data.Traversable     as T (Traversable, traverse)

import Tct.Common.Pretty    as PP
import Tct.Core.Certificate (Certificate, unbounded)
import Tct.Core.Processor


-- | A 'ProofNode' stores the necessary information to construct a proof from the application of a 'Processor'.
data ProofNode p = ProofNode
  { problem   :: Problem p
  , processor :: p
  , proof     :: ProofObject p
  }

-- | The 'ProofTree' is the obtained from applying a 'Strategy' to a problem.
--
-- During evaluation 'Open' nodes store the open (sub-)problems,
-- 'NoProgress' nodes result from failing 'Processor' application, and
-- 'Progress' nodes result from successfull 'Processor' application.
data ProofTree l where
  Open       :: l -> ProofTree l
  NoProgress :: Processor p => ProofNode p -> ProofTree l -> ProofTree l
  Progress   :: Processor p => ProofNode p -> CertificateFn p -> Forking p (ProofTree l) -> ProofTree l

-- | Lifts the result of a 'Processor' application (see 'solve') to 'ProofTree'. Informally we have:
--
-- prop> 'Fail'    -> 'NoProgress'
-- prop> 'Success' -> 'Progress'
resultToTree :: Processor p => Problem p -> p -> Result p -> ProofTree (Problem p)
resultToTree prob p (Fail po)                    = NoProgress (ProofNode prob p po) (Open prob)
resultToTree prob p (Success subprobs po certfn) = Progress (ProofNode prob p po) certfn (Open `fmap` subprobs)

-- | Computes the 'Certificate' of a 'ProofTree'.
-- 'Open' nodes have the 'Certificate' 'unboundend.
-- prop> certificate pt = collectCertificate (const unbounded `fmap` pt)
certificate :: ProofTree l -> Certificate
certificate pt = collectCertificate $ const unbounded `fmap` pt

-- | Computes the 'Certificate' of 'ProofTree'.
collectCertificate :: ProofTree Certificate -> Certificate
collectCertificate (Open c)                      = c
collectCertificate (NoProgress _ subtree)        = certificate subtree
collectCertificate (Progress _ certfn' subtrees) = certfn' (certificate `fmap` subtrees)

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
isOpen = null . open

-- | Checks if there are no 'Open' nodes in the 'ProofTree'.
-- prop> isCloses = not . isOpen
isClosed :: ProofTree l -> Bool
isClosed = not . isOpen


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

instance Processor p => Pretty (ProofNode p) where
  pretty (ProofNode prob p po) =
    text "Considered Problem:" <$$> indent 2 (pretty prob)
    <$$> text "Applied Processor:" <$$> indent 2 (text $ name p)
    <$$> text "Proof:" <$$> indent 2 (pretty po)

filler :: Doc
filler = text "-------------------------------------------------------------------------------"

instance Pretty l => Pretty (ProofTree l) where
  pretty (Open l) =
    empty
    <$$> filler
    <$$> text "?" <+> pretty l
  pretty (NoProgress pn pt) =
    empty
    <$$> filler
    <$$> pretty pn
    <$$> pretty pt
    <$$> pretty (certificate pt)
  pretty (Progress pn _ pts) =
    empty
    <$$> filler
    <$$> pretty pn
    <$$> indent 2 (vcat $ pretty `fmap` toList pts)

