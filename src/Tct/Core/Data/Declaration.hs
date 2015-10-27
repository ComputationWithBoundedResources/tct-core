-- | A declaration associates meta-information such as name and description, to a function.
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
  , flag
  ) where


import Data.Typeable
import           Data.List              (intercalate)

import qualified Tct.Core.Common.Pretty as PP
import           Tct.Core.Data.Types
import qualified Tct.Core.Common.Parser as P


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

instance WithName (Declaration c) where withName (Decl _ h f as) n' = Decl n' h f as
instance WithHelp (Declaration c) where withHelp (Decl n _ f as) h' = Decl n h' f as

-- | Specifies the default function of a 'Declaration'.
type family DeflFun c where
  DeflFun ('[] :-> r) = r
  DeflFun (Argument 'Optional a ': as :-> r) = DeflFun (as :-> r)
  DeflFun (Argument 'Required a ': as :-> r) = a -> DeflFun (as :-> r)

-- | Specifies the default function of a 'Declaration'.
-- The default function instantiates all optional arguments with their default value.
class DefF c where
  deflFun :: Declaration c -> DeflFun c

instance DefF ('[] :-> f) where
  deflFun (Decl _ _ f _) = f

instance (DefF (as :-> r)) => DefF (Argument 'Optional a ': as :-> r) where
  deflFun (Decl n h f (HCons a as)) = deflFun (Decl n h (f (argDefault a)) as)
  deflFun Decl{}                    = error "Tct.Core.Declaration.deflFun: something ubelievable happened"

instance DefF (as :-> r) => DefF (Argument 'Required a ': as :-> r) where
  deflFun (Decl n h f (HCons _ as)) = \ a' -> deflFun (Decl n h (f a') as)
  deflFun Decl{}                    = error "Tct.Core.Declaration.deflFun: something ubelievable happened"


--- * arguments ------------------------------------------------------------------------------------------------------
-- instance Functor (Argument r) where
--   _ `fmap` ar@ReqArg{} =
--     ReqArg { argName = argName ar, argDomain = argDomain ar, argHelp = argHelp ar }
--   f `fmap` ar@OptArg{} =
--     OptArg { argName = argName ar, argDomain = argDomain ar, argHelp = argHelp ar, argDefault = f (argDefault ar) }

-- | Generic argument with name "arg" and domain "<arg>".
arg :: String -> String -> [String] -> SParser t -> Argument 'Required t
arg n d h p = SimpleArg (ArgMeta {argName_ = n, argDomain_ = d, argHelp_ = h}) p

-- | Transforms a required argument to an optional by providing a default value.
optional :: Typeable t => Argument 'Required t -> t -> Argument 'Optional t
optional ar a = OptArg ar a

-- | Wraps an argument into 'Maybe'.
some :: Argument 'Required a -> Argument 'Required (Maybe a)
some a = SomeArg a

-- * instances
type Nat = Int

-- | Specifies a natural argument with domain "<nat>".
nat :: String -> [String] -> Argument 'Required Nat
nat n h = arg n "nat" h P.nat

-- | Specifies a bool argument with domain "<bool>".
bool :: String -> [String] -> Argument 'Required Bool
bool n h = arg n "bool" h P.bool

string :: String -> [String] -> Argument 'Required String
string n h = arg n "string" h P.identifier

-- | Specifies a strategy argument with name "strategy" and domain "<strategy>".
strat :: Declared i o => String -> [String] -> Argument 'Required (Strategy i o)
strat n h = StrategyArg (ArgMeta { argName_ = n, argDomain_ = "strategy", argHelp_ = h }) 

-- TODO: MS: generate domain from bounded instance
flag :: (Show t, Typeable t, Bounded t, Enum t) => String -> [String] -> Argument 'Required t
flag n h = FlagArg (ArgMeta { argName_ = n, argDomain_ = "flag", argHelp_ = h })

withDomain :: Argument r a -> [String] -> Argument r a
withDomain ar ns = case ar of
  (SimpleArg a p) -> SimpleArg (k a) p
  (FlagArg a)     -> FlagArg (k a)
  (StrategyArg a) -> StrategyArg (k a)
  (SomeArg a)     -> SomeArg (withDomain a ns) 
  (OptArg a t)    -> OptArg (withDomain a ns) t
  where k a = a { argDomain_ = intercalate "|" ns }

instance WithName ArgMeta        where withName ar n = ar { argName_ = n }
instance WithName (Argument r a) where withName ar n = setArgMeta (flip withName n) ar

instance WithHelp ArgMeta        where withHelp ar n = ar { argHelp_ = n }
instance WithHelp (Argument r a) where withHelp ar n = setArgMeta (flip withHelp n) ar

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
          mkSynopsis (_ , ad, _, Nothing) = PP.angles (PP.text ad)
          mkSynopsis (an, ad, _, _)       = PP.brackets $ PP.char ':' PP.<> PP.text an PP.<+> PP.angles (PP.text ad)
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

instance (Show a, ArgsInfo as) => ArgsInfo (Argument r a ': as) where
  argsInfo (HCons a as) = argsInfo' a :argsInfo as where
    argsInfo' :: Show t => Argument f t -> (String, String, [String], Maybe String)
    argsInfo' (OptArg b t) = let m = argMeta b in (argName_ m, argDomain_ m, argHelp_ m, Just $ show t)
    argsInfo' b            = let m = argMeta b in (argName_ m, argDomain_ m, argHelp_ m, Nothing)

