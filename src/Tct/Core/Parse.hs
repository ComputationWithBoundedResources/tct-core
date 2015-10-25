-- | This module provides common 'SParsable' instances and the strategy parser.
module Tct.Core.Parse
  (
  ParsableArgs (..)
  , declaration
  , strategy
  , strategyDeclarations
  , strategyFromString
  ) where


import           Data.Data                 (Typeable)
import           Data.Dynamic              (fromDynamic, toDyn)
import           Data.List                 (sortBy)
import           Data.Maybe                (fromMaybe)
import qualified Text.Parsec.Expr          as PE
import qualified Tct.Core.Data.Declaration as D
import qualified Tct.Core.Data.Strategy    as S
import           Tct.Core.Data.Types
import           Tct.Core.Common.Parser


curried :: f ~ Uncurry (args :-> Ret args f) => f -> HList args -> Ret args f
curried f HNil         = f
curried f (HCons a as) = curried (f a) as

declaration :: ParsableArgs args => Declaration (args :-> r) -> SParser r
declaration (Decl n _ f as) = do
  _    <- try (symbol n)
  opts <- many (choice (map try (mkOptParser as)))
  vs   <- mkArgParser as opts
  return (curried f vs)

strategyDeclarations :: [StrategyDeclaration i o] -> SParser (Strategy i o)
strategyDeclarations ds =
  choice [ declaration d | SD d <- sortBy k ds ]
    -- MS: there is an issue when declarations have only optional arguments and a common prefix
    -- as decl will always be successfull; so we sort the list in rev. lex order
    where k (SD d1) (SD d2) = compare (D.declName d2) (D.declName d1)

strategy :: [StrategyDeclaration i i] -> SParser (Strategy i i)
strategy ds = PE.buildExpressionParser table strat <?> "stratgy"
  where
    strat =
      parens (strategy ds)
      <|> predefined
      <?> "expression"
    predefined = strategyDeclarations ds
    -- MA:TODO: todo, add more
    table = [ [unary "try" S.try , unary "force" S.force ]
            , [unary "es"  S.es ]
            , [binary "<|>" (S..<|>) PE.AssocRight,   binary "<||>" (S..<||>) PE.AssocRight ]
            , [binary ">>>" (S..>>>) PE.AssocRight, binary ">||>" (S..>||>) PE.AssocRight ] ]
    binary name fun = PE.Infix (do{ reserved name; return fun })
    unary name fun = PE.Prefix (do{ reserved name; return fun })


instance ParsableArgs '[] where
  mkOptParser _   = []
  mkArgParser _ _ = return HNil

instance (Typeable a, ParsableArgs as) => ParsableArgs (Argument 'Optional a ': as) where
  mkOptParser (HCons (a@OptArg{}) as) = ( (\ v -> (argName a, toDyn v)) <$> pa a ) : mkOptParser as
    where
      pa :: Argument 'Optional a -> SParser a
      pa t = symbol (':' : argName t) >> optParser t
  mkArgParser (HCons a as) ls = do
    let v = fromMaybe (argDefault a) (lookup (argName a) ls >>= fromDynamic)
    vs <- mkArgParser as ls
    return (HCons v  vs)

instance (ParsableArgs as) => ParsableArgs (Argument 'Required a ': as) where
  mkOptParser (HCons _ as) = mkOptParser as
  mkArgParser (HCons a as) ls = do
    v  <- reqParser a
    vs <- mkArgParser as ls
    return (HCons v vs)

parse' :: Declared i o => SParser (Strategy i o)
parse' = strategyDeclarations decls

reqParser :: Argument 'Required t -> SParser t
reqParser (SimpleArg _ p) = p
reqParser (StrategyArg _) = parse'
reqParser (FlagArg _)     = enum
reqParser (SomeArg a)     = (try (symbol "none") >> return Nothing) <|> (Just <$> reqParser a) <?> "maybe"

optParser :: Argument 'Optional t -> SParser t
optParser (OptArg a _) = reqParser a

strategyFromString :: [StrategyDeclaration i i] ->  String -> Either ParseError (Strategy i i)
strategyFromString ds = runParser (do {_ <- whiteSpace; p <- strategy ds; eof; return p}) () "supplied string"

