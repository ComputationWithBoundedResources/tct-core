-- | 

module Tct.Core.Declaration.Parse where

import           Text.Parsec (getState,(<?>),runParser,eof, ParseError, many, choice, (<|>), oneOf, letter, alphaNum, string, try)
import qualified Text.Parsec.Expr as PE
import qualified Text.Parsec.Language as PL
import qualified Text.Parsec.Token as PT
import           Data.Dynamic (toDyn, fromDynamic)
import           Data.Maybe (fromMaybe)
import           Data.Data (Typeable)
import           Control.Applicative ((<$>))
import qualified Tct.Core.Declaration as D
import qualified Tct.Core.Strategy as S
import           Tct.Core.Types

strategyTP :: PT.TokenParser st
strategyTP = PT.makeTokenParser style
  where
    style =
      PL.emptyDef { PT.commentStart   = "{-"
                  , PT.commentEnd     = "-}"
                  , PT.commentLine    = "--"
                  , PT.nestedComments = True
                  , PT.identStart     = letter
                  , PT.identLetter    = alphaNum <|> oneOf "_'"
                  , PT.reservedOpNames= ["try", "force"]
                  , PT.reservedNames  = [">>>", ">||", "<>", "<||>" ]
                  , PT.caseSensitive  = True
                  }

lexeme :: SParser prob a -> SParser prob a 
lexeme = PT.lexeme strategyTP

symbol :: String -> SParser prob String
symbol = PT.symbol strategyTP

reserved :: String -> SParser prob ()
reserved = PT.reservedOp strategyTP

braces :: SParser prob a -> SParser prob a
braces = PT.braces strategyTP

parens :: SParser prob a -> SParser prob a
parens = PT.parens strategyTP

natural :: SParser prob Int
natural = fromInteger `fmap` PT.natural strategyTP

bool :: SParser prob Bool
bool = try (symbol "True" >> return True) <|> (symbol "False" >> return False)

integer :: SParser prob Integer
integer = PT.natural strategyTP

identifier :: SParser prob String
identifier = PT.identifier strategyTP

whiteSpace :: SParser prob ()
whiteSpace = PT.whiteSpace strategyTP


curried :: f ~ Uncurry (args :-> Ret args f) => f -> HList args -> Ret args f
curried f HNil = f
curried f (HCons a as) = curried (f a) as

decl :: (PA prob args) => Declaration (args :-> r) -> SParser prob r
decl (Decl n _ f as) = do
  _ <- try (string n)
  opts <- many (choice (map try (mkOptParsers as)))
  vs <- mkArgParser as opts 
  return (curried f vs)
  
strategy :: SParser prob (Strategy prob)
strategy = PE.buildExpressionParser table strat <?> "strategy"
  where
    strat = parens strategy
          <|> predefined
          <?> "expression"
    predefined :: SParser prob (Strategy prob)      
    predefined = do
      decls <- getState
      choice [ decl d | SD d <- decls ]
      
    table = [ [binary ">>>" (S.Then) PE.AssocLeft, binary ">||>" (S.ThenPar) PE.AssocLeft ]
            , [binary "<>" (S.Alt) PE.AssocLeft, binary "<||>" (S.OrFaster)   PE.AssocLeft ]
            ]
    binary  name fun assoc = PE.Infix (do{ reserved name; return fun }) assoc

instance PA prob '[] where
  mkOptParsers  _ = []
  mkArgParser _ _ = return HNil

instance (Typeable a, SParsable prob a, PA prob as) => PA prob (Argument Optional a ': as) where
  mkOptParsers (HCons (a@OptArg{}) as) = ( (\ v -> (argName a, toDyn v)) <$> pa a ) : mkOptParsers as
    where
      pa :: SParsable prob a => Argument Optional a -> SParser prob a
      pa _ = symbol (':' : argName a) >> parseS                                            
  mkArgParser (HCons a as) ls = do
    v  <- return (fromMaybe (argDefault a) (lookup (argName a) ls >>= fromDynamic))
    vs <- mkArgParser as ls
    return (HCons v  vs)

instance (SParsable prob a, PA prob as) => PA prob (Argument Required a ': as) where
  mkOptParsers (HCons ReqArg{} as) = mkOptParsers as
  mkArgParser (HCons _ as) ls = do
    v  <- lexeme parseS
    vs <- mkArgParser as ls
    return (HCons v  vs)


instance SParsable prob D.Nat where parseS = natural
instance SParsable prob Bool where parseS = bool
instance SParsable prob (Strategy prob) where parseS = strategy
instance (Typeable a, SParsable prob a) => SParsable prob (Maybe a) where parseS = Just `fmap` parseS

strategyFromString :: [StrategyDeclaration prob] -> String -> Either ParseError (Strategy prob)
strategyFromString ls = runParser (do {_ <- whiteSpace; p <- strategy; eof; return p}) ls "supplied string"
