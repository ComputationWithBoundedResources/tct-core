-- |

module Tct.Core.Data.Declaration.Parse
  (
  SParsable
  , ParsableArgs
  , strategyFromString
  , decl -- TODO: MS: rename
  ) where


import           Control.Applicative       ((<$>))
import           Data.Data                 (Typeable)
import           Data.Dynamic              (fromDynamic, toDyn)
import           Data.List                 (sortBy)
import           Data.Maybe                (fromMaybe)
import           Tct.Core.Common.Parser
import qualified Tct.Core.Data.Declaration as D
import qualified Tct.Core.Data.Strategy    as S
import           Tct.Core.Data.Types
import qualified Text.Parsec.Expr          as PE

-- FIXME: MS: resolve cyclic dependencies; combinators dependes on this module; but we also want; exhaustively and co

curried :: f ~ Uncurry (args :-> Ret args f) => f -> HList args -> Ret args f
curried f HNil         = f
curried f (HCons a as) = curried (f a) as

decl :: (ParsableArgs i o args) => Declaration (args :-> r) -> SParser i o r
decl (Decl n _ f as) = do
  _    <- try (symbol n)
  opts <- many (choice (map try (mkOptParser as)))
  vs   <- mkArgParser as opts
  return (curried f vs)

strategy :: SParser i i (Strategy i i)
strategy = PE.buildExpressionParser table strat <?> "stratgy"
  where
    strat =
      parens strategy
      <|> predefined
      <?> "expression"
    predefined :: SParser i o (Strategy i o)
    predefined = do
      decls <- getState
      -- MS: there is an issue when declarations have only optional arguments and a common prefix
      -- as decl will always be successfull; so we sort the list in rev. lex order
      choice [ decl d | SD d <- sortBy k decls ]
        where k (SD d1) (SD d2)= compare (D.declName d2) (D.declName d1)

    table = [ [unary "try" (S.Trying True) ,      unary "force" (S.Trying False) ]
            , [unary "es" (\s1 -> s1 `S.Then` S.Trying True s1) ]
            , [binary "<>" S.Alt PE.AssocRight,   binary "<||>" S.OrFaster PE.AssocRight ]
            , [binary ">>>" S.Then PE.AssocRight, binary ">||>" S.ThenPar PE.AssocRight ] ]
    binary name fun = PE.Infix (do{ reserved name; return fun })
    unary name fun = PE.Prefix (do{ reserved name; return fun })

instance ParsableArgs i o '[] where
  mkOptParser _   = []
  mkArgParser _ _ = return HNil

instance (Typeable a, SParsable i o a, ParsableArgs i o as) => ParsableArgs i o (Argument Optional a ': as) where
  mkOptParser (HCons (a@OptArg{}) as) = ( (\ v -> (argName a, toDyn v)) <$> pa a ) : mkOptParser as
    where
      pa :: SParsable i o a => Argument Optional a -> SParser i o a
      pa _ = symbol (':' : argName a) >> parseS
  mkArgParser (HCons a as) ls = do
    let v = fromMaybe (argDefault a) (lookup (argName a) ls >>= fromDynamic)
    vs <- mkArgParser as ls
    return (HCons v  vs)

instance (SParsable i o a, ParsableArgs i o as) => ParsableArgs i o (Argument Required a ': as) where
  mkOptParser (HCons ReqArg{} as) = mkOptParser as
  mkArgParser (HCons _ as) ls     = do
    v  <- lexeme parseS
    vs <- mkArgParser as ls
    return (HCons v vs)

instance SParsable i o D.Nat          where parseS = nat
instance SParsable i o Bool           where parseS = bool
instance SParsable i o String         where parseS = identifier
instance SParsable i i (Strategy i i) where parseS = strategy
instance (Typeable a, SParsable i o a) => SParsable i o (Maybe a) where
  parseS = (try (symbol "none") >> return Nothing) <|> Just `fmap` parseS <?> "maybe"

strategyFromString :: [StrategyDeclaration i i] -> String -> Either ParseError (Strategy i i)
strategyFromString ls = runParser (do {_ <- whiteSpace; p <- strategy; eof; return p}) ls "supplied string"

