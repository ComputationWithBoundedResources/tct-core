module Tct.Core.ProofTree
  (
    ProofNode (..)
  , ProofTree (..)
  , certificate
  , collectCertificate
  , resultToTree
  , progress
  , open
  , isOpen
  , isClosed
  ) where


import Control.Applicative as A ((<$>))
import Data.Foldable as F (Foldable, foldr, foldMap, toList)
import Data.Traversable as T (Traversable, traverse)

import Tct.Core.Certificate (Certificate, unbounded)
import Tct.Core.Processor
import Tct.Pretty as PP


data ProofNode p = ProofNode
  { problem   :: Problem p
  , processor :: p
  , proof     :: ProofObject p }

data ProofTree l where
  Open       :: l -> ProofTree l
  NoProgress :: Processor p => ProofNode p -> ProofTree l -> ProofTree l
  Progress   :: Processor p => ProofNode p -> CertificateFn p -> Forking p (ProofTree l) -> ProofTree l

resultToTree :: Processor p => Problem p -> p -> Result p -> ProofTree (Problem p)
resultToTree prob p (Fail po)                    = NoProgress (ProofNode prob p po) (Open prob)
resultToTree prob p (Success subprobs po certfn) = Progress (ProofNode prob p po) certfn (Open `fmap` subprobs)

certificate :: ProofTree l -> Certificate
certificate pt = collectCertificate $ const unbounded `fmap` pt

collectCertificate :: ProofTree Certificate -> Certificate
collectCertificate (Open c)                      = c
collectCertificate (NoProgress _ subtree)        = certificate subtree
collectCertificate (Progress _ certfn' subtrees) = certfn' (certificate `fmap` subtrees)

progress :: ProofTree l -> Bool
progress (Open _)          = False
progress (NoProgress _ pt) = progress pt
progress (Progress {})     = True

open :: ProofTree l -> [l]
open = F.foldr (:) []

isOpen :: ProofTree l -> Bool
isOpen = null . open

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

