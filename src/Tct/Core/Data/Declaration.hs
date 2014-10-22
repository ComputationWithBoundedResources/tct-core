{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
-- | 

module Tct.Core.Data.Declaration
  ( 
  declName
  , declHelp
  , declFun
  , declArgs
  
  , defaultFun

  -- * StrategyDeclaration
  , StrategyDeclaration
  

  -- * Arguments
  , Argument (..)
  , ArgFlag (..)
  , OneTuple (..)
  , arg
  , optional
  , some

  -- * Instances  
  , Nat 
  , nat
  , bool
  , strat
  ) where


import           Tct.Core.Data.Types
import qualified Tct.Core.Common.Pretty as PP


instance WithName (Declaration c) where withName (Decl _ h f as) n' = Decl n' h f as
instance WithHelp (Declaration c) where withHelp (Decl n _ f as) h' = Decl n h' f as

-- | Returns the name of a 'Declarationi'.
declName :: Declaration c -> String
declName (Decl n _ _  _) = n

-- | Returns the description of a 'Declarationi'.
declHelp :: Declaration c -> [String]
declHelp (Decl _ h _  _) = h

-- | Returns the function of a 'Declarationi'.
declFun :: Declaration (args :-> r) -> Uncurry (ArgsType args :-> r)
declFun (Decl _ _ f  _) = f

-- | Returns the arguments of a 'Declarationi'.
declArgs :: Declaration (args :-> r) -> HList args
declArgs (Decl _ _ _ as) = as


-- | Specifies the default function of a 'Declaration'.
type family DefaultFun c where
  DefaultFun ('[] :-> r) = r
  DefaultFun (Argument Optional a ': as :-> r) = DefaultFun (as :-> r)
  DefaultFun (Argument Required a ': as :-> r) = a -> DefaultFun (as :-> r)

-- | Specifies the default function of a 'Declaration'.
-- The default function instantiates all optional arguments with their default value.
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


-- arguments ---------------------------------------------------------------------------------------------------------
instance Functor (Argument r) where
  _ `fmap` ar@ReqArg{} = 
    ReqArg { argName = argName ar, argDomain = argDomain ar, argHelp = argHelp ar }
  f `fmap` ar@OptArg{} = 
    OptArg { argName = argName ar, argDomain = argDomain ar, argHelp = argHelp ar, argDefault = f (argDefault ar) }

-- | Generic argument with name "arg" and domain "<arg>".
arg :: Argument Required a
arg = ReqArg {argName = "arg", argDomain = "<arg>", argHelp = []}

-- | Transforms a required argument to an optional by providing a default value.
optional :: Argument Required a -> a -> Argument Optional a
optional ar a = OptArg {argName = argName ar, argDomain = argDomain ar, argHelp = argHelp ar, argDefault = a }

-- | Wraps an argument into 'Mayb'.
some :: Argument r a -> Argument r (Maybe a)
some ar = Just `fmap` ar { argDomain = argDomain ar ++ "|none"}

-- * instances
type Nat = Int

-- | Specifies a natural argument with name "nat" and domain "<nat>".
nat :: Argument Required Nat
nat = arg { argName = "nat", argDomain = "<nat>" }

-- | Specifies a bool argument with name "bool" and domain "<bool>".
bool :: Argument Required Bool
bool = arg { argName = "bool", argDomain = "<bool>" }

-- | Specifies a strategy argument with name "strategy" and domain "<strategy>".
strat :: Argument Required (Strategy prob)
strat = arg { argName = "strategy", argDomain = "<strategy>" }

instance WithName (Argument r a) where withName ar n = ar { argName = n}
instance WithHelp (Argument r a) where withHelp ar n = ar { argHelp = n}


-- StrategyDeclaration

instance PP.Pretty (StrategyDeclaration prob) where
  pretty (SD s) = PP.pretty s

instance ArgsInfo args => PP.Pretty (Declaration (args :-> c)) where
  pretty (Decl n h _ as) = PP.vcat $
    [ theName
    , theLine
    , if null h then PP.empty else PP.indent 4 theHelp PP.<$$> PP.empty
    , PP.indent 4 theSynopsis 
    , PP.empty ]
    ++ (if null opts then [] else [PP.indent 2 theOptArgs])
    ++ (if null reqs then [] else [PP.indent 2 theReqArgs])
    ++ [PP.empty]

    where
      theName = PP.text "Strategy" PP.<+> PP.text n 
      theLine = PP.text $ replicate (length $ "Strategy " ++ n) '-'
      theHelp = PP.paragraph (unlines h)
      theSynopsis = PP.text "Synopsis: " PP.<+> PP.text n PP.<+> PP.hsep (map mkSynopsis info)
        where
          mkSynopsis (_ , ad, _, Nothing) = PP.dquotes (PP.text ad)
          mkSynopsis (an, ad, _, _)       = PP.brackets $ PP.char ':' PP.<> PP.text an PP.<+> PP.text ad
      theOptArgs = PP.text "Optional:" PP.<$$> PP.indent 2 (PP.vcat (map mkArgsInfo opts))
      theReqArgs = PP.text "Required:" PP.<$$> PP.indent 2 (PP.vcat (map mkArgsInfo reqs))

      info = argsInfo as
      (opts,reqs) = foldr k ([],[]) info
        where
          k a@(_,_,_,Nothing) (os,rs) = (os,a:rs)
          k a                 (os,rs) = (a:os,rs)
      mkArgsInfo (an, _, ah, Nothing) = mkArgsInfo' an ah
      mkArgsInfo (an, _, ah, Just s)  = mkArgsInfo' an ah PP.<$$> PP.indent 2 (PP.text "default:" PP.<+> PP.text s)
      mkArgsInfo' an ah               = PP.text an PP.<> if null ah then PP.empty else PP.empty PP.<$$> PP.indent 2 (PP.paragraph $ unlines ah)

instance ArgsInfo '[] where
  argsInfo HNil = []

instance (Show a, ArgsInfo (as)) => ArgsInfo (Argument r a ': as) where
  argsInfo (HCons a@ReqArg{} as) = (argName a, argDomain a, argHelp a, Nothing) :argsInfo as
  argsInfo (HCons a@OptArg{} as) = (argName a, argDomain a, argHelp a, Just (show $ argDefault a)) :argsInfo as

