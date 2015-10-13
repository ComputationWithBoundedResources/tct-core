-- | This module provides the 'ProofTree' type.
-- 'ProofTree' stores the open problems and the proofs of applying 'Processor' instances to problems.
module Tct.Core.Data.ProofTree
  (
  -- * ProofTree
  ProofNode (..)
  , ProofTree (..)
  , open
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



import Prelude hiding (mapM,any)
import Data.Monoid
import           Data.Traversable
import Control.Applicative
import           Data.Foldable             as F (Foldable, foldMap, foldr, toList, any)


import qualified Tct.Core.Common.Pretty    as PP
import           Tct.Core.Data.Certificate (Certificate, timeLB, timeUB, unbounded)
import           Tct.Core.Data.Types

-- | Returns the 'Open' nodes of a 'ProofTree'.
open :: ProofTree l -> [l]
open = F.foldr (:) []

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
isFailure (Success _ _ pts) = F.any isFailure pts
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

-- ppNodeShort :: (PP.Pretty (ProofObject a), Show a) => ProofNode a -> PP.Doc
-- ppNodeShort (ProofNode p _ po) = PP.vcat
--   [ PP.text "Applied Processor:"  PP.<$$> ind (PP.text $ show p)
--   , PP.text "Proof:"              PP.<$$> ind (PP.pretty po) ]
--   where ind = PP.indent 2

ppProofTree' :: (Int,[Int]) -> (prob -> PP.Doc) -> Bool -> ProofTree prob -> PP.Doc
ppProofTree' is ppProb _ pt@(Open l) = PP.vcat
  [ ppHeader pt is "Open"
  , PP.indent 4 (ppProb l) ]
ppProofTree' is _ _ f@(Failure _) = ppHeader f is "Failure"
    
ppProofTree' (i,is) ppProb detailed pt@(Success pn _ pts) = PP.vcat
  [ ppHeader pt (i,is) "Success"
  , PP.indent 4 (ppProofNode pn)
  , PP.indent (if length (take 2 ppts) < 2 then 0 else 2) (PP.vcat ppts) ]
    where ppts = (\(j,t) -> ppProofTree' (j, is++[i]) ppProb detailed t) `fmap` zip [1..] (F.toList pts)

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

