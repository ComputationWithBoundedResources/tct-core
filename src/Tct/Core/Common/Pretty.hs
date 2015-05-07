-- | This module re-exports "Text.PrettyPrint.ANSI.Leijen" (<http://hackage.haskell.org/package/ansi-wl-pprint>) and
-- provides some additional pretty-printing functions.
module Tct.Core.Common.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , Align (..)
  , table

  , list, list'
  , tupled, tupled'
  , set, set'
  , itemise, itemise'
  , enumerate, enumerate'
  , listing, listing'

  , paragraph
  , display
  , putPretty
  ) where


import           Control.Arrow                ((***))
import qualified Data.Foldable                as F
import           Data.List                    (transpose)
import qualified Data.Set                     as S
import           Text.PrettyPrint.ANSI.Leijen hiding (list, tupled)


-- | Sets 'table' alignment.
data Align = AlignLeft | AlignRight | AlignCenter deriving (Show, Eq)

-- | Provides tabular pretty-printing.
table :: [(Align, [Doc])] -> Doc
table cols = vcat [ pprow row | row <- rows]
  where
    rows     = transpose [ [ (al,len,c) | c <- cs ] | (al,len,cs) <- cols']
    -- rows'     = [ [(al, h, c) | (al,c) <- r ]
    --             | r <- rows
    --             , let h = maximum $ 0 : [length c | (_, c) <- r]]
    cols'     = [ (al,len,cs')
                | (al,cs) <- cols
                , let cs' = [ lines $ show c | c <- cs ++ replicate (numrows - length cs) empty]
                      len = maximum $ 0: concat [ map length c | c <- cs']]
    numrows = maximum $ 0 : [length cs | (_,cs) <- cols ]
    pprow row =
      vcat [ hcat [ text $ pad al len c | (al, len, c) <- rl ]
           | rl <- transpose [ [(al, len, s) | s <- ls ++ replicate (height - length ls) ""] | (al, len, ls) <- row] ]
      where
        height = maximum $ 0 : [length ls | (_, _, ls) <- row]
        pad AlignLeft len s   = s ++ ws (len - length s)
        pad AlignRight len s  = ws (len - length s) ++ s
        pad AlignCenter len s = ws l ++ s ++ ws r
          where
            diff = len - length s
            l    = floor $ fromIntegral diff / (2.0 :: Double)
            r    = diff - l
        ws n = replicate n ' '


-- | Generalised version of Pretty.list.
list :: F.Foldable f => f Doc -> Doc
list = encloseSep lbracket rbracket comma . F.toList

-- | prop> list' xs == list (fmap pretty xs)
list' :: (F.Foldable f, Pretty a) =>  f a -> Doc
list' = list . fmap pretty . F.toList

-- | Generalised version of Pretty.tupled.
tupled :: F.Foldable f => f Doc -> Doc
tupled = encloseSep lparen rparen comma . F.toList

-- | > tupled' xs == tupled (fmap pretty xs)
tupled' :: (F.Foldable f, Pretty a) =>  f a -> Doc
tupled' = tupled . fmap pretty . F.toList

-- | @set ds@ encloses @ds@ in braces and seperates them using commas.
set :: F.Foldable f => f Doc -> Doc
set = encloseSep lbrace rbrace comma . F.toList

-- | > set' xs == set (nub . fmap pretty xs)
set' :: (F.Foldable f, Pretty a, Ord a) =>  f a -> Doc
set' = set . fmap pretty . F.toList . F.foldr S.insert S.empty

-- | @itemise bullet docs@. Provides an itemise environment.
itemise :: F.Foldable f => Doc -> f Doc -> Doc
itemise d ds = table [( AlignRight, replicate (length ds') d), (AlignLeft, ds')]
  where ds' = F.toList ds

-- | > itemise' d ds == itemise d (fmap pretty ds)
itemise' :: (F.Foldable f, Pretty a) => Doc -> f a -> Doc
itemise' d = itemise d . fmap pretty . F.toList

-- | Provides a listing environment.
listing :: F.Foldable f => f (Doc, Doc) -> Doc
listing xs = table [( AlignLeft, is), (AlignLeft, ds)]
  where (is,ds) = unzip (F.toList xs)

-- | > listing' ds == listing (fmap (pretty *** pretty) ds)
listing' :: (F.Foldable f, Pretty a, Pretty b) => f (a,b) -> Doc
listing' = listing . map (pretty *** pretty) . F.toList

-- | Provides an enumerate environment.
enumerate :: F.Foldable f => f Doc -> Doc
enumerate ds = table [(AlignRight, enumeration), (AlignLeft, ds')]
  where
    ds'         = F.toList ds
    enumeration = take (length ds') [ int i <> text ".) " | i <- [1..]]

-- | enumerate' ds == enumerate (fmap pretty ds)
enumerate' :: (F.Foldable f, Pretty a) => f a -> Doc
enumerate' = enumerate . fmap pretty . F.toList


-- | Constructs a paragraph, respecting newline characters.
paragraph :: String -> Doc
paragraph s = vcat [fillSep [text w | w <- words l] | l <- lines s]

-- | Default 'Doc' rendering.
display :: Doc -> String
display d = displayS (renderPretty 0.9 1000 d) ""

-- | Pretty print to stdout.
putPretty :: Pretty a => a -> IO ()
putPretty = putStrLn . display . pretty

