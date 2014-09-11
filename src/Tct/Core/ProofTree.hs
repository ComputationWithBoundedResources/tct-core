-- | This module provides the 'ProofTree' type.
-- 'ProofTree' stores the open problems and the proofs of applying 'Processor' instances to problems.
module Tct.Core.ProofTree
  (
  -- * ProofTree
  ProofNode (..)
  , ProofTree (..)
  -- * Certification
  , collectCertificate
  , certificate
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


-- | A 'ProofNode' stores the necessary information to construct a (formal) proof from the application of a 'Processor'.
data ProofNode p = ProofNode
  { problem   :: Problem p
  , processor :: p
  , proof     :: ProofObject p }

-- | A 'ProofTree' is constructed by applying a 'Tct.Core.Strategy' to a problem.
-- During evaluation
--
-- * 'Open' nodes store the open (sub-)problems,
-- * 'NoProgress' nodes result from failing 'Processor' applications, and
-- * 'Progress' nodes result from successfull 'Processor' application.
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

instance Processor p => Pretty (ProofNode p) where
  pretty (ProofNode prob p po) =
    text "Considered Problem:" <$$> indent 2 (pretty prob)
    <$$> text "Applied Processor:" <$$> indent 2 (text $ name p)
    <$$> text "Proof:" <$$> indent 2 (pretty po)

instance Pretty l => Pretty (ProofTree l) where
  pretty = prettyProofTree

prettyProofTree :: Pretty prob => ProofTree prob -> PP.Doc
prettyProofTree pt =
  paragraph "The considered (sub-)prooftree is" <+> status pt <+> paragraph "and has certificate"
  <$$> pretty (certificate pt) <> dot
  <$$> linebreak <> indent 2 (pp pt) where

  pp (Open l) = block "* Open ***" body empty where
    body = paragraph "Following problem is open:" <$$> indent 2 (pretty l)

  pp (NoProgress (ProofNode prob p _) spt) =
    block "*** NoProgress ***" body after where
      body =
        paragraph "We consider following open problem:"
        <$$> indent 2 (pretty prob)
        <$$> linebreak <> paragraph "We fail to apply:"
        <$$> indent 2 (text $ name p)
        -- <$$> paragraph "The reason is:"
        -- <$$> indent 2 (pretty po)
      after = linebreak
        <$$> paragraph "We continue with the proof."
        <$$> linebreak <> pp spt

  pp t@(Progress (ProofNode prob p po) _ spts) =
    block "*** Progress ***" body after where
      body =
        paragraph "We consider following open problem:"
        <$$> indent 2 (pretty prob)
        <$$> linebreak <> paragraph "We successfully apply"
        <$$> indent 2 (text $ name p)
        <$$> linebreak <> paragraph "yielding" <+> pprobs (map pretty $ open t)
        <$$> linebreak <> paragraph "and obtaining certificate"
        <$$> indent 2 (pretty $ certificate t)
        <$$> linebreak <> linebreak <> paragraph "The proof is:"
        <$$> linebreak <> indent 2 (pretty po) <> pprobs' (map pprob ps)
      after = if null ps
        then empty
        else linebreak <> indent 4 (
          linebreak <> paragraph "We continue with the proof."
          <$$> linebreak <> vcat (punctuate linebreak [ pp spt | spt <- toList spts]))
      ps = toList spts

  status t = text $ if isClosed t then "closed" else "open"

  block header body after = paragraph header <$$> filler (length header) <$$> indent 2 body <> after

  pprobs [] = linebreak <> indent 2 (paragraph "no open problems.")
  pprobs [p] =
    paragraph "following open problem:"
    <$$> indent 2 p
  pprobs ps =
    paragraph "following open problems:"
    <$$> indent 2 (vcat (punctuate linebreak ps))

  pprobs' [] = empty
  pprobs' [p] = linebreak <$$> indent 2 (paragraph "Yielding following open problem:" <$$> indent 2 p)
  pprobs' ps  = linebreak <$$> indent 2 (paragraph "Yielding following open problems:" <$$> indent 2 (vcat (punctuate linebreak ps)))

  pprob (Open l)          = pretty l
  pprob (NoProgress pn _) = pretty $ problem pn
  pprob (Progress pn _ _) = pretty $ problem pn

  filler i = text $ replicate i '-'

