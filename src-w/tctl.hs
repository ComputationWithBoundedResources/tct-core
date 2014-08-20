{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad              (liftM)
import           Data.List
import qualified Data.Map                   as M
import qualified Options.Applicative        as O
import           Text.Read                  (readMaybe)

import           SmtLib.Logic.Core
import           SmtLib.Logic.Int
import           SmtLib.SMT                 as SMT hiding (asserts)
import           SmtLib.Solver              as SMT

-- load Tct library
import           Tct
import           Tct.Core
import           Tct.Error
import           Tct.Pretty
import           Tct.Processors.Combinators


-- Example Configuration File -----------------------------------------------


-- the TctMode specifies the problem and the strategies to consider when executing tct

wmode :: TctMode WProblem (Maybe Int)
wmode = TctMode
  { modeParser                       = note (TctParseError "WMode problem") . readMaybe
  , modeStrategies                   =
      [ SomeProc $ OrderProc Nothing
      , SomeProc $ SplitProc
      , SomeProc $ strat1
      , SomeProc $ strat2 ]
  , modeDefaultStrategy              = SomeProc $ strat1
  , modeOptions                      = O.optional $ O.option (O.long "int")
  , modeModifyer                     = const }



-- instantiate Tct configuration - updates strategies, defaultstrategy ... wrt to the mode
-- same technique as used before but we have to provide the problem type and some additional problem specific information
main :: IO ()
main = tctl $ applyMode wmode


-- problem definition
--- problem specific properties like rule selection have to be stored in the problem definition
--- alternatively we could provide an additional type for the TctMode and for the Tct monad
type Term        = [Char]
data Rule        = Rule { lhs :: Term, rhs :: Term } deriving (Show, Read)
newtype WProblem = WProblem { rules :: [Rule] } deriving (Show, Read)

-- some necessary instances for pretty printing the proof
instance Pretty Rule where
  pretty r = string (lhs r) <+> string "->" <+> string (rhs r)
instance Pretty WProblem where
  pretty (WProblem rs) = vcat $ map pretty rs


-- processors are defined similar as before
data OrderProof = OrderProof String deriving Show
instance Pretty OrderProof where pretty =  text . show

data OrderProcessor = OrderProc (Maybe Int) deriving Show

instance Processor OrderProcessor where
  -- the type of the proof
  type ProofObject OrderProcessor = OrderProof
  -- map processor to a problem
  type Problem OrderProcessor     = WProblem
  -- define the branching of the proofnode in the prooftree; a judgment has no successor
  type Forking OrderProcessor     = Judgement
  name                            = const "OrderProcessor"
  description                     = const "tries to find a weighting function and a ordering st..."
  solve _ w                       = do
    res <- liftIO $ entscheide w
    return $ case res of
      Sat a -> Success Judgement (OrderProof $ show a) (judgement $ timeUBCert linear)
      _     -> Fail (OrderProof "fail")

-- example smt encoding: find weighting function w:Char->Int that defines a proper order on the rules
lhssf,rhssf :: WProblem -> [Term]
lhssf w = lhs `map` rules w
rhssf w = rhs `map` rules w

encodeWeights :: Monad m => WProblem -> SMT m (M.Map Char Literal)
encodeWeights w = snd `liftM` memo (mapM nvarm $ concat (lhssf w ++ rhssf w))

encodeSums :: Monad m => WProblem -> SMT m [Literal]
encodeSums w = mapM (const nvar) [0 ..  (length (rules w)*2 - 1)]

entscheide :: WProblem -> IO (Sat (M.Map Char Constant, [Constant]))
entscheide w = do
  res :: Sat (M.Map Char Constant, [Constant]) <- SMT.solve minismt $ do
    setLogic "QF_NIA"
    -- construct encoder functions
    encoderWeights <- encodeWeights w
    encoderSums    <- encodeSums w

    -- encode sums
    let
      weight = fm . (encoderWeights M.!)
      sums   = fm . (encoderSums !!)
      asserts f = mapM (assert . f)

    let encodeSum (i,term) = sums i  .== fm (bigPlus $  weight `map` term)
    _ <- asserts encodeSum (zip ilhss lhss)
    _ <- asserts encodeSum (zip irhss rhss)

    -- encode rule order; lhs >= rhs
    let encodeOrder (i,j) = sums i .>= sums j
    _ <- asserts encodeOrder (zip ilhss irhss)

    -- encode rules order; find ordering rhs x > lhs y >= rhs y >= ...
    -- compute permutations; make components; break cycle
    let
      l1 = [ (a,b) | a <- permutations ilhss,  b <- permutations irhss ]
      l2 = map (tail . uncurry zip) l1
      tryOrder = ((\(i,j) -> sums i .<  sums j) `map`)
    _ <- assert $ bigOr ((bigAnd . tryOrder) `map` l2)
    return $ decode (encoderWeights, encoderSums)
  return res
  where
    ilhss = take (length $ rules w) [0,2..]
    irhss = take (length $ rules w) [1,3..]
    lhss = lhssf w
    rhss = rhssf w

-- to make make the processor parsable from command line we have to provide a parser for its arguments
-- we use the Options.Applicative library to do this; (also allows to define descriptions for the arguments to provide the help text)
-- default instances of precessors are necessary ! (eg. the defaultProcessor for timeout is; TimeoutProc Nothing Nothing FailProc)
-- not as sophisticated/safe as in tct2 but should be enough; eg something like $ "TimeoutProcessor --inT 10 OrderProcessor" is possible
instance ParsableProcessor OrderProcessor where
  args _ _ = Args $ SomeProc `O.liftA` (OrderProc O.<$> O.optional (O.option (O.long "bound")))
  -- default implementation of the parser takes processor name and its defined arguments into account eg. "OrderProcessor --bound 3"

-- and another one
data SplitProof = SplitProof String String | SplitFail String deriving Show
instance Pretty SplitProof where pretty = text . show

data SplitProcessor = SplitProc deriving Show

instance Processor SplitProcessor where
  type ProofObject SplitProcessor = SplitProof
  type Problem SplitProcessor     = WProblem
  type Forking SplitProcessor     = Pair
  name                            = const "SplitProcessor"
  description                     = const "Splits the problem in half; supproblems are multiplied"
  solve _ w
    | len < 2   = return $ Fail (SplitFail "not enough elemnts" )
    | otherwise = return $ Success  mkPair (SplitProof "as" "bs") (\(Pair(ca,cb)) -> timeUBCert $ timeUB ca `mult` timeUB cb )
    where
      len    = length $ rules w
      mkPair = Pair (WProblem as, WProblem bs)
        where (as,bs) = splitAt (floor $ (toRational len) / 2) $ rules w

instance ParsableProcessor SplitProcessor where
  -- default instance is Unit Argument


-- define custom strategies;
-- a strategy composes processors : eg. p1 >>> p2,or p1 <|> p2
-- strategies are automatically lifted to processors : eg timeoutIn 10 (p1 >>> p2)
-- processors have to be lifted to strategies : some == Proc . SomeProc
-- all in all; similar as in tct2

order,split :: Strategy WProblem
order = Proc $ SomeProc $ OrderProc Nothing
split = Proc $ SomeProc $ SplitProc

direct :: Int -> Strategy WProblem
direct i = some $ timeoutIn i order

-- strategy allows us to define a customstrategy;
-- we have to provide a (uniqu) id and a parser for its arguments
-- additionally we have to provide a default instance
strat1 :: CustomStrategy Int WProblem
strat1 = strategy "direct" pargs direct $ 10
  where pargs = O.option (O.long "timeout")

withSplit :: Maybe Int -> Strategy WProblem
withSplit Nothing  = exhaustively (try order >>> split)
withSplit (Just n) = some $ timeoutIn n $ exhaustively (try order >>> split)

strat2 :: CustomStrategy (Maybe Int) WProblem
strat2 = strategy "withSplit" pargs withSplit $ Nothing
  where pargs = O.optional $ O.option (O.long "timeout")


-- example calls

-- ./tct "direct --timeout 0" problem
-- ./tct "direct --timeout 1" problem
-- ./tct "TimeoutProcessor --inT 5 OrderProcessor" problem



