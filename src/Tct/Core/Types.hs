module Tct.Core.Types 
    ( TctM (..)
    , Fork
    , ProofData
    , Processor (..)
    , SomeProcessor
    , Pr   

type TctM = IO


type Fork t = (Foldable t, Functor t, Traversable t)


type ProofData d = (Xml.Xml d, PP.Pretty d)

class (ProofData (ProofObject p), Fork (Forking p))  => Processor p where
    type ProofObject p :: *
    type Forking p :: * -> *
    name :: p -> String
    options :: p -> O.Options [SomeProcessor] p
    solve :: p -> P.Problem -> TctM (Result p)

data SomeProcessor where
    SomeProcessor :: Processor p => p -> SomeProcessor

type CertificateFn p = Forking p C.Certificate -> C.Certificate

data Result p = 
    Fail { proofData :: ProofObject p }
  | Sucess { subTrees :: Forking p ProofTree
           , proofData :: ProofObject p
           , certificateFn :: CertificateFn p }

data ProofNode p = 
    ProofNode { problem :: P.Problem
              , processor :: p
              , proof :: ProofObject p}
                 
data ProofTree where 
    Open :: P.Problem -> ProofTree
    NoProgress :: Processor p => ProofNode p -> ProofTree -> ProofTree
    Progress :: Processor p => ProofNode p -> CertificateFn p -> Forking p ProofTree -> ProofTree 

