module Tct.Processors.Combinators where

import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)
import           Tct.Options
import qualified Tct.Core as C
import Tct.Xml as Xml
import Tct.Pretty as Pretty

data TimeoutP p = TimeoutP { untilT :: Maybe Int, inT :: Maybe Int, procT :: p }
  deriving (Show, Typeable)

data TimeoutProof p
  = Timeout Int
  | NoTimeout (C.ProofObject p)
                      
instance C.Processor p => Show (TimeoutProof p) where
  show (Timeout i)    = "Timeout " ++ show i
  show (NoTimeout po) = "NoTimeout (" ++ show po ++ ")"

--instance (C.Processor p) => Xml.Xml (TimeoutProof p) where
    --toXml (Timeout i)     = Xml.elt "timeout" [] [Xml.int i]
    --toXml (NoTimeout po) = toXml obj
                                    
instance C.Processor p => Pretty.Pretty (TimeoutProof p) where
  pretty (Timeout i)    = Pretty.paragraph ("Computation aborted after a timeout of " ++ show i ++ " seconds")
  pretty (NoTimeout po) = Pretty.pretty po

instance C.Processor p => C.Processor (TimeoutP p) where
  type ProofObject (TimeoutP p) = TimeoutProof p
  type Forking (TimeoutP p)     = C.Forking p
  type Problem (TimeoutP p)     = C.Problem p
  name p = C.name (procT p)
  options _ =
      [ option { keyword = "until"
                , meaning = (\ f n -> f { untilT = Just n}) <#> nat
                , help = []}
      , option { keyword = "in"
                , meaning = (\ f n -> f { inT = Just n}) <#> nat } ]

  solve proc prob = do
    running <- C.runningTime `fmap` C.askStatus prob
    let i = fromMaybe 0 (inT proc)
        u = fromMaybe 0 (untilT proc)
        t = max 0 (min i (u - running))
    mr <- C.timeout t (C.solve (procT proc) prob)
    return $ case mr of
      Nothing -> C.Fail (Timeout t)
      Just (C.Fail p) -> C.Fail (NoTimeout p)
      Just r@(C.Success {}) -> C.Success
        { C.subProblems   = C.subProblems r
        , C.proofData     = NoTimeout (C.proofData r)
        , C.certificateFn = C.certificateFn r }

