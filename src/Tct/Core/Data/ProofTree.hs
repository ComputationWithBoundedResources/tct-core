-- | This module provides the 'ProofTree' type.
-- 'ProofTree' stores the open problems and the proofs of applying 'Processor' instances to problems.
module Tct.Core.Data.ProofTree
  (
  -- * ProofTree
  ProofNode (..)
  , ProofTree (..)
  , open
  , size
  , flatten
  , substitute
  , substituteM
  -- * Certification
  , certificate
  , certificateWith
  -- * Properites
  , isOpen
  , isFailure
  , isClosed
  , isProgressing
  -- * Output
  , ppProofTree
  , ppProofTreeLeafs
  , ppDetailedProofTree
  ) where


import qualified Data.Foldable             as F (toList)

import qualified Tct.Core.Common.Pretty    as PP
import           Tct.Core.Data.Certificate (Certificate, timeLB, timeUB, unbounded)
import           Tct.Core.Data.Types


-- | Returns the 'Open' nodes of a 'ProofTree'.
open :: ProofTree l -> [l]
open = foldr (:) []

-- | Returns the number of nodes of a 'ProofTree'.
size :: ProofTree l -> Int
size (Open _)          = 1
size (Failure _)       = 1
size (Success _ _ pts) = 1 + sum (size <$> pts)

-- | Substitute the open leaves of a proof tree according to the given function
substituteM :: (Functor m, Monad m) => (l -> m (ProofTree k)) -> ProofTree l -> m (ProofTree k)
substituteM s (Open l)             = s l
substituteM _ (Failure r)          = return (Failure r)
substituteM s (Success pn cf pts) = Success pn cf <$> mapM (substituteM s) pts

substitute :: (l -> ProofTree k) -> ProofTree l -> ProofTree k
substitute f  (Open l)           = f l
substitute _ (Failure r)         = Failure r
substitute f (Success pn cf pts) = Success pn cf (substitute f `fmap` pts)

-- | Flattens a nested prooftree.
flatten :: ProofTree (ProofTree l) -> ProofTree l
flatten = substitute id

-- | Computes the 'Certificate' of 'ProofTree'.
collectCertificate :: ProofTree Certificate -> Certificate
collectCertificate (Open c)                     = c
collectCertificate Failure{}                    = unbounded
collectCertificate (Success _ certfn' subtrees) = certfn' (collectCertificate `fmap` subtrees)

-- | Computes the 'Certificate' of a 'ProofTree'.
-- 'Open' nodes have the 'Certificate' 'unboundend'.
--
-- prop> certificate pt = collectCertificate (const unbounded `fmap` pt)
certificate :: ProofTree l -> Certificate
certificate pt = collectCertificate $ const unbounded `fmap` pt

-- | Computes the 'Certificate' of a 'ProofTree'.
-- 'Open' nodes have the 'Certificate' provided certificate'.
--
-- prop> certificate pt cert = collectCertificate (const cert `fmap` pt)
certificateWith :: ProofTree l -> Certificate -> Certificate
certificateWith pt cert = collectCertificate $ const cert `fmap` pt


-- | Checks if the 'ProofTree' contains a 'Failure' node.
isFailure :: ProofTree l -> Bool
isFailure Failure{}         = True
isFailure (Success _ _ pts) = any isFailure pts
isFailure _                 = False

-- | Checks that the 'ProofTree' does not contain a 'Failure' node
-- and not consist of a single 'Open' node
isProgressing :: ProofTree l -> Bool
isProgressing (Open _) = False
isProgressing p = not (isFailure p)

-- | Checks if there exists 'Open' nodes in the 'ProofTree'.
isOpen :: ProofTree l -> Bool
isOpen = not . isClosed

-- | Checks if there are no 'Open' nodes in the 'ProofTree'.
--
-- prop> isClosed = not . isOpen
isClosed :: ProofTree l -> Bool
isClosed = null . open

instance Functor ProofTree where
  f `fmap` Open l             = Open (f l)
  _ `fmap` (Failure r)        = Failure r
  f `fmap` Success pn cns pts = Success pn cns ((f `fmap`) `fmap` pts)

instance Foldable ProofTree where
  f `foldMap` Open l          = f l
  _ `foldMap` Failure{}       = mempty
  f `foldMap` Success _ _ pts = (f `foldMap`) `foldMap` pts

instance Traversable ProofTree where
  f `traverse` Open l  = Open <$> f l
  _ `traverse` Failure r = pure (Failure r)
  f `traverse` Success pn cfn pts = Success pn cfn <$> (f `traverse`) `traverse` pts

instance Show (ProofTree l) where
  show _ = "showTree"


--- * Pretty Printing ------------------------------------------------------------------------------------------------

ppProofNode :: Processor p => ProofNode p -> PP.Doc
ppProofNode (ProofNode p prob po) = PP.vcat
  [ PP.text "Considered Problem:" PP.<$$> ind (PP.pretty prob)
  , PP.text "Applied Processor:"  PP.<$$> ind (PP.text $ show p)
  , PP.text "Proof:"              PP.<$$> ind (PP.pretty po) ]
  where ind = PP.indent 2

ppProofTree' :: (Int,[Int]) -> (prob -> PP.Doc) -> Bool -> ProofTree prob -> PP.Doc
ppProofTree' is ppProb _ pt@(Open l) = PP.vcat
  [ ppHeader pt is "Open"
  , PP.indent 4 (ppProb l) ]
ppProofTree' is _ _ f@(Failure r) =
  ppHeader f is "Failure"
  PP.<$$> PP.indent 2 (PP.pretty r)

ppProofTree' (i,is) ppProb detailed pt@(Success pn _ pts) =
  PP.vcat [ ppHeader pt (i,is) "Success", PP.indent 4 (ppProofNode pn), ppSubTrees (F.toList pts) ]
  where
    ppSubTrees []  = PP.empty
    ppSubTrees [t] = ppProofTree' (i+1,is) ppProb detailed t
    ppSubTrees ls  = PP.vcat [ ppProofTree' (j,is++[i]) ppProb detailed t
                             | (j,t) <- zip [1..] ls]

ppHeader :: ProofTree l -> (Int, [Int]) -> String -> PP.Doc
ppHeader pt (i,is) s =
  PP.text "***"
  PP.<+> PP.cat (PP.punctuate PP.dot $ PP.int `fmap` (is++[i]))
  PP.<+> PP.text s
  PP.<+> PP.brackets (PP.pretty (cert $ certificate pt))
  PP.<+> PP.text " ***"
  where cert c = (timeLB c, timeUB c)

ppProofTree :: (l -> PP.Doc) -> ProofTree l -> PP.Doc
ppProofTree pp pt =
  ppProofTree' (1,[]) pp False pt
  PP.<> if null (F.toList pt) then PP.empty else
    PP.empty
    PP.<$$> PP.text "Following problems could not be solved:"
    PP.<$$> PP.indent 2 (ppProofTreeLeafs pp pt)

ppDetailedProofTree :: (l -> PP.Doc ) -> ProofTree l -> PP.Doc
ppDetailedProofTree pp pt =
  ppProofTree' (1,[]) pp True pt
  PP.<> if null (F.toList pt) then PP.empty else
    PP.empty
    PP.<$$> PP.text "Following problems could not be solved:"
    PP.<$$> PP.indent 2 (ppProofTreeLeafs pp pt)

ppProofTreeLeafs :: (l -> PP.Doc) -> ProofTree l -> PP.Doc
ppProofTreeLeafs pp = PP.vcat . map pp . F.toList

instance PP.Pretty prob => PP.Pretty (ProofTree prob) where
  pretty = ppProofTree PP.pretty

