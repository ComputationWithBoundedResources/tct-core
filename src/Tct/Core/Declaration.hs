{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
-- | 

module Tct.Core.Declaration
  ( 
   
  declName
  , declHelp
  , declFun
  , declArgs
  
  , defaultFun
  , defaultDecl
  , app1

  -- * StrategyDeclaration
  , StrategyDeclaration
  , liftP
  

  -- * Arguments
  , arg
  , optional
  , some

  -- * Instances  
  , Nat 
  , nat
  , bool
  , strat
  ) where


import           Tct.Core.Types

instance WithName (Declaration c) where withName (Decl _ h f as) n' = Decl n' h f as
instance WithHelp (Declaration c) where withHelp (Decl n _ f as) h' = Decl n h' f as

declName :: Declaration c -> String
declName (Decl n _ _  _) = n

declHelp :: Declaration c -> [String]
declHelp (Decl _ h _  _) = h

declFun :: Declaration (args :-> r) -> Uncurry (ArgsType args :-> r)
declFun (Decl _ _ f  _) = f

declArgs :: Declaration (args :-> r) -> HList args
declArgs (Decl _ _ _ as) = as


-- default fun
type family DefaultFun c where
  DefaultFun ('[] :-> r) = r
  DefaultFun (Argument Optional a ': as :-> r) = DefaultFun (as :-> r)
  DefaultFun (Argument Required a ': as :-> r) = a -> DefaultFun (as :-> r)

class DefF c where
  defaultFun :: Declaration c -> DefaultFun c

instance DefF ('[] :-> f) where
  defaultFun (Decl _ _ f _) = f

instance (DefF (as :-> r)) => DefF (Argument Optional a ': as :-> r) where
  defaultFun (Decl n h f (HCons a as)) = defaultFun (Decl n h (f (argDefault a)) as)
  defaultFun Decl{}                    = error "Tct.Core.Declaration.defaultFun: something ubelievable happened"

instance DefF (as :-> r) => DefF (Argument Required a ': as :-> r) where
  defaultFun (Decl n h f (HCons _ as)) = \ a' -> defaultFun (Decl n h (f a') as)
  defaultFun Decl{}                    = error "Tct.Core.Declaration.defaultFun: something ubelievable happened"

-- default declaration
class DefD args args' where
  defaultDecl :: Declaration (args :-> r) ->  Declaration (args' :-> r)

instance DefD '[] '[] where
  defaultDecl (Decl n h f as) = Decl n h f as

instance (DefD as as') => DefD (Argument Optional a ': as) as' where
  defaultDecl (Decl n h f (HCons a as)) = defaultDecl (Decl n h (f (argDefault a)) as)
  defaultDecl Decl{}                    = error "Tct.Core.Declaration.defaultDecl: something ubelievable happened"

instance (DefD as as') => DefD (Argument Required a ': as) (Argument Required a ': as') where
  defaultDecl Decl{} = error "Tct.Core.Declaration.defaultDecl: something ubelievable happened"


-- processor lifting
class LiftP arg where
  liftP :: (Processor p, ProcessorArgs p ~ arg) => Declaration (arg :-> p) ->  Declaration (arg :-> Strategy (Problem p))

instance LiftP '[] where
  liftP (Decl n h f as) = Decl n h (Proc f) as

instance LiftP as => LiftP (a ': as) where
  liftP (Decl n h f as) = liftP (Decl n h f as)


class App1 args a args' where
  app1 :: Declaration (args :-> r) -> a -> Declaration (args' :-> r)

instance App1 (Argument Optional a ': as) a as where
  app1 (Decl n h f (HCons _ as)) a = Decl n h (f a) as
  app1 Decl{} _                    = undefined

instance App1 (Argument Required a ': as) a as where
  app1 (Decl n h f (HCons _ as)) a = Decl n h (f a) as
  app1 Decl{} _            = undefined



    
-- arguments ----------------------------------------------------------------------
instance Functor (Argument r) where
  f `fmap` ar@OptArg{} = OptArg { argName = argName ar, argHelp = argHelp ar, argDefault = f (argDefault ar) }
  _ `fmap` ar@ReqArg{} = ReqArg { argName = argName ar, argHelp = argHelp ar }

arg :: Argument Required a
arg = ReqArg {argName = "arg", argHelp = []}

optional :: Argument Required a -> a -> Argument Optional a
optional ar a = OptArg {argName = argName ar, argHelp = argHelp ar, argDefault = a }

some :: Argument r a -> Argument r (Maybe a)
some ar = Just `fmap` ar

-- * instances
type Nat = Int

nat :: Argument Required Nat
nat = arg { argName = "nat" }

bool :: Argument Required Bool
bool = arg { argName = "bool" }

strat :: Argument Required (Strategy prob)
strat = arg { argName = "processor" }

instance WithName (Argument r a) where withName ar n = ar { argName = n}
instance WithHelp (Argument r a) where withHelp ar n = ar { argHelp = n}


-- StrategyDeclaration

instance WithName (StrategyDeclaration prob) where withName (SD (Decl _ h f as)) n' = SD $ Decl n' h f as
instance WithHelp (StrategyDeclaration prob) where withHelp (SD (Decl n _ f as)) h' = SD $ Decl n h' f as


