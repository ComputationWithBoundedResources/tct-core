{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
-- | 

module Tct.Core.Data.Declaration
  ( 
  declName
  , declHelp
  , declFun
  , declArgs
  , deflFun

  -- * StrategyDeclaration
  , StrategyDeclaration
  

  -- * Arguments
  , Argument (..)
  , ArgFlag (..)
  , OneTuple (..)
  , arg
  , optional
  , some
  , withDomain

  -- * Instances  
  , Nat 
  , nat
  , bool
  , string
  , strat
  ) where


import           Data.List              (intercalate)

import qualified Tct.Core.Common.Pretty as PP
import           Tct.Core.Data.Types


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
type family DeflFun c where
  DeflFun ('[] :-> r) = r
  DeflFun (Argument Optional a ': as :-> r) = DeflFun (as :-> r)
  DeflFun (Argument Required a ': as :-> r) = a -> DeflFun (as :-> r)

-- | Specifies the default function of a 'Declaration'.
-- The default function instantiates all optional arguments with their default value.
class DefF c where
  deflFun :: Declaration c -> DeflFun c

instance DefF ('[] :-> f) where
  deflFun (Decl _ _ f _) = f

instance (DefF (as :-> r)) => DefF (Argument Optional a ': as :-> r) where
  deflFun (Decl n h f (HCons a as)) = deflFun (Decl n h (f (argDefault a)) as)
  deflFun Decl{}                    = error "Tct.Core.Declaration.deflFun: something ubelievable happened"

instance DefF (as :-> r) => DefF (Argument Required a ': as :-> r) where
  deflFun (Decl n h f (HCons _ as)) = \ a' -> deflFun (Decl n h (f a') as)
  deflFun Decl{}                    = error "Tct.Core.Declaration.deflFun: something ubelievable happened"

-- liftD :: (r -> s) -> Declaration('[] :-> r) -> Declaration('[] :-> s)
-- liftD g (Decl n h f as) = Decl n h (g $ f) as

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

-- | Wraps an argument into 'Maybe'.
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

string :: Argument Required String
string = arg { argName = "string", argDomain = "<string>" }

-- | Specifies a strategy argument with name "strategy" and domain "<strategy>".
strat :: Argument Required (Strategy i o)
strat = arg { argName = "strategy", argDomain = "<strategy>" , argHelp = ["The sub-strategy to apply."]}

withDomain :: Argument r a -> [String] -> Argument r a
withDomain ar ns = ar { argDomain = k $ intercalate "|" ns }
  where k s = '<':s++">"

instance WithName (Argument r a) where withName ar n = ar { argName = n }
instance WithHelp (Argument r a) where withHelp ar n = ar { argHelp = n }


-- StrategyDeclaration

instance PP.Pretty (StrategyDeclaration i o) where
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

