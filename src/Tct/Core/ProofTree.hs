module Tct.Core.ProofTree 
    (
     children
    , certificate
    , modifyOpen
    , openProblems
    , isOpen
    , isClosed
    ) where

children :: ProofTree -> [ProofTree]
children (Open _) = []
children (NoProgress _ t) = [t]
children (Progress _ _ subtrees) = toList subtrees

certificate :: ProofTree -> C.Certificate
certificate Open {} = C.unbounded
certificate (NoProgress _ subtree) = certificate subtree
certificate (Progress _ certfn subtrees) = certfn (fmap certificate subtrees)


modifyOpen :: Monad m => (P.Problem -> m ProofTree) -> ProofTree -> m ProofTree
modifyOpen f (Open p) = f p
modifyOpen f (NoProgress n subtree) = NoProgress n `liftM` (modifyOpen f subtree)
modifyOpen f (Progress n certfn subtrees) = 
    Progress n certfn `liftM` (modifyOpen f `T.mapM` subtrees)

openProblems :: ProofTree -> [P.Problem]
openProblems (Open prob) = [prob]
openProblems tree = openProblems `Prelude.concatMap` children tree

isOpen :: ProofTree -> Bool
isOpen = null . openProblems

isClosed :: ProofTree -> Bool
isClosed = not . isOpen

    
