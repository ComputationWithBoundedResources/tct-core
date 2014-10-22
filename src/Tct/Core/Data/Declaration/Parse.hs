-- | 

module Tct.Core.Data.Declaration.Parse 
  (
  SParsable
  , ParsableArgs
  , strategyFromString
  ) where


import           Control.Applicative       ((<$>))
import           Data.Data                 (Typeable)
import           Data.Dynamic              (fromDynamic, toDyn)
import           Data.Maybe                (fromMaybe)
import           Tct.Core.Common.Parser
import qualified Tct.Core.Data.Declaration as D
import qualified Tct.Core.Data.Strategy    as S
import           Tct.Core.Data.Types
import qualified Text.Parsec.Expr          as PE


curried :: f ~ Uncurry (args :-> Ret args f) => f -> HList args -> Ret args f
curried f HNil = f
curried f (HCons a as) = curried (f a) as

decl :: (ParsableArgs prob args) => Declaration (args :-> r) -> SParser prob r
decl (Decl n _ f as) = do
  _    <- try (symbol n)  
  opts <- many (choice (map try (mkOptParsers as)))
  vs   <- mkArgParser as opts
  return (curried f vs)
  
strategy :: SParser prob (Strategy prob)
strategy = PE.buildExpressionParser table strat <?> "stratgy"
  where
    strat = 
      predefined
      <?> "expression"
    predefined :: SParser prob (Strategy prob)      
    predefined = do
      decls <- getState
      choice [ decl d | SD d <- decls ]
      
    table = [ [binary ">>>" S.Then PE.AssocRight, binary ">||>" S.ThenPar PE.AssocRight ]
            , [binary "<>" S.Alt PE.AssocRight,   binary "<||>" S.OrFaster PE.AssocRight ]
            ]
    binary name fun = PE.Infix (do{ reserved name; return fun })

instance ParsableArgs prob '[] where
  mkOptParsers  _ = []
  mkArgParser _ _ = return HNil

instance (Typeable a, SParsable prob a, ParsableArgs prob as) => ParsableArgs prob (Argument Optional a ': as) where
  mkOptParsers (HCons (a@OptArg{}) as) = ( (\ v -> (argName a, toDyn v)) <$> pa a ) : mkOptParsers as
    where
      pa :: SParsable prob a => Argument Optional a -> SParser prob a
      pa _ = symbol (':' : argName a) >> parseS                                            
  mkArgParser (HCons a as) ls = do
    let v = fromMaybe (argDefault a) (lookup (argName a) ls >>= fromDynamic)
    vs <- mkArgParser as ls
    return (HCons v  vs)

instance (SParsable prob a, ParsableArgs prob as) => ParsableArgs prob (Argument Required a ': as) where
  mkOptParsers (HCons ReqArg{} as) = mkOptParsers as
  mkArgParser (HCons _ as) ls = do
    v  <- lexeme parseS
    vs <- mkArgParser as ls
    return (HCons v  vs)


instance SParsable prob D.Nat where parseS = nat
instance SParsable prob Bool where parseS = bool
instance SParsable prob (Strategy prob) where parseS = strategy
instance (Typeable a, SParsable prob a) => SParsable prob (Maybe a) where 
  parseS = (try (symbol "none") >> return Nothing) <|> Just `fmap` parseS <?> "maybe"

strategyFromString :: [StrategyDeclaration prob] -> String -> Either ParseError (Strategy prob)
strategyFromString ls = runParser (do {_ <- whiteSpace; p <- strategy; eof; return p}) ls "supplied string"

