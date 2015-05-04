module Tct.Core.Processor.Empty 
  ( empty )
  where

import qualified Tct.Core.Common.Pretty   as PP
import           Tct.Core.Common.SemiRing (zero)
import qualified Tct.Core.Common.Xml      as Xml
import qualified Tct.Core.Data            as T


data Empty prob = Empty (prob -> Bool)

instance Show (Empty prob) where
  show _ = "EmptyProcessor"

data EmptyProof
  = EmptyProblem
  | OpenProblem
  deriving Show

instance PP.Pretty EmptyProof where
  pretty EmptyProblem = PP.text "The problem is already closed. The intended complexity is O(1)."
  pretty OpenProblem  = PP.text "The problem is still open."

instance Xml.Xml EmptyProof where
  toXml EmptyProblem  = Xml.elt "closed" []
  toXml OpenProblem   = Xml.elt "open" []

  toCeTA EmptyProblem = Xml.elt "rIsEmpty" []
  toCeTA _            = Xml.unsupported

instance T.ProofData prob => T.Processor (Empty prob) where
  type ProofObject (Empty prob) = EmptyProof
  type I (Empty prob)     = prob
  type O (Empty prob)     = prob
  type Forking (Empty prob)     = T.Judgement

  solve p@(Empty f) prob = return . T.resultToTree p prob $
    if f prob
      then T.Success T.Judgement EmptyProblem (T.judgement zero)
      else T.Fail OpenProblem

empty :: T.ProofData i => (i -> Bool) -> T.Strategy i i 
empty = T.Proc . Empty

