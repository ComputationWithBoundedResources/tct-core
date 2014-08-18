module Tct.Core.Processor 
  ( 
    Result (..)
  , Processor (..)
  , ParsableProcessor (..)
  , SomeProcessor (..)
  , Arguments (..)

  , readAnyProc
  , readAnyProcMaybe

  , Fork
  , ProofData
  , CertificateFn
  
  , ErroneousProcessor (..)
  , ErroneousProof (..)
  ) where


import           Data.Foldable as F
import           Data.Traversable as T
import           Data.Either (isRight)
import           Data.List as L (find)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mempty)
import qualified Options.Applicative as O

import           Tct.Core.TctM
import           Tct.Core.Forks (Id(..))
import qualified Tct.Core.Certificate as C
import qualified Tct.Pretty as PP
import qualified Tct.Xml as Xml


-- Processor ----------------------------------------------------------------- 
type Fork t      = (Foldable t, Functor t, Traversable t)
type ProofData d = (PP.Pretty d, Show d)
--type ProofData d = (Xml.Xml d, PP.Pretty d, Show d)

type CertificateFn p = Forking p C.Certificate -> C.Certificate

data Result p
  = Fail { proofData :: ProofObject p }
  | Success 
    { subProblems   :: Forking p (Problem p) 
    , proofData     :: ProofObject p
    , certificateFn :: CertificateFn p }

class (Show p, ProofData (ProofObject p), ProofData (Problem p), Fork (Forking p)) => Processor p where
  type ProofObject p :: *
  type Problem p     :: *
  type Forking p     :: * -> *
  type Forking p     = Id
  name               :: p -> String
  description        :: p -> String
  description        = name
  solve              :: p -> Problem p -> TctM (Result p)

data Arguments a = Unit | Args a

class Processor p => ParsableProcessor p where
  args          :: p -> [SomeProcessor (Problem p)] -> Arguments (O.Parser (SomeProcessor (Problem p)))
  readProcessor :: p -> [SomeProcessor (Problem p)] -> String -> Either String (SomeProcessor (Problem p))
  args _ _              = Unit
  readProcessor p ps ss = case args p ps of
    Unit       -> readUnit
    Args pargs -> readArgs pargs
    where
      readUnit
        | name p == ss = Right (SomeProc p)
        | otherwise   = Left $ "unit argument (" ++ name p ++ ss ++ ")"
      readArgs pargs 
        | name p == t =
          case O.execParserPure (O.prefs mempty) (O.info pargs O.briefDesc) ts of
            O.Success a   -> Right a
            O.Failure err -> Left $ "optParser error (" ++ show err ++ "," ++ show ss ++ ")"
            _             -> Left $ "optParser completion error (" ++ show ss ++ ")"
        | otherwise = Left $ name p ++ ss
        where (t:ts) = words ss

readAnyProc :: 
  (ParsableProcessor (SomeProcessor prob), Problem (SomeProcessor prob) ~ prob) 
  => [SomeProcessor prob] -> String -> Either String (SomeProcessor prob)
readAnyProc rs s =  def `fromMaybe` L.find isRight (map (\r -> readProcessor r rs s) rs)
  where def = Left $ "readAnyProc: ("  ++ s ++ ")"

readAnyProcMaybe :: 
  (ParsableProcessor (SomeProcessor t), Problem (SomeProcessor t) ~ t) => 
  [SomeProcessor t] -> String -> Maybe (SomeProcessor t)
readAnyProcMaybe rs s = either (const Nothing) Just $ readAnyProc rs s


data SomeProcessor :: * -> * where
  SomeProc :: (Processor p, ParsableProcessor p) => p -> SomeProcessor (Problem p)

instance Show (SomeProcessor prob) where
  show (SomeProc p) = show p


-- Error Processor ----------------------------------------------------------- 
data ErroneousProof p = ErroneousProof IOError p deriving Show

instance Processor p => Xml.Xml (ErroneousProof p) where
  toXml (ErroneousProof err p) = 
    Xml.elt "error" [] [ Xml.elt "processor" [] [Xml.text (name p)]
                       , Xml.elt "message" [] [Xml.text (show err)] ]

instance Processor p => PP.Pretty (ErroneousProof p) where 
  pretty (ErroneousProof err p) = 
    PP.text "Processor" PP.<+> PP.squotes (PP.text (name p)) PP.<+> PP.text "signalled the following error:"
    PP.<$$> PP.indent 2 (PP.paragraph (show err))

data ErroneousProcessor p = ErroneousProc IOError p deriving Show

instance Processor p => Processor (ErroneousProcessor p) where
  type ProofObject (ErroneousProcessor p) = ErroneousProof p
  type Problem (ErroneousProcessor p)     = Problem p
  name (ErroneousProc err p)              = name p ++ "[error: " ++ show err ++ "]"
  solve (ErroneousProc err p) _           = return (Fail (ErroneousProof err p))

instance Processor p => ParsableProcessor (ErroneousProcessor p) where

