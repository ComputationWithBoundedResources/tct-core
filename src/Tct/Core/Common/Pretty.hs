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

-- FIXME: MS: sometimes we obtain unwanted linebreaks when using table (enumerate, listing, itemise)
-- >ppProofTreeLeafs pp = PP.vcat . map pp . F.toList (works fine)
-- >ppProofTreeLeafs pp = PP.enumerate . map pp . F.toList (not really)
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
                -- , let cs' = [ lines $ show c | c <- cs ++ replicate (numrows - length cs) empty]
                , let cs' = [ lines $ display c | c <- cs ++ replicate (numrows - length cs) empty]
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

-- | Like Pretty.encloseSep, but does not perform any vertical alignment.
encloseWith :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseWith l r p ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> hcat (zipWith (<>) (l : repeat (softbreak <> p)) ds) <> r

-- | Pretty print a list.
list :: F.Foldable f => f Doc -> Doc
list = encloseWith lbracket rbracket comma . F.toList

-- | prop> list' xs == list (fmap pretty xs)
list' :: (F.Foldable f, Pretty a) =>  f a -> Doc
list' = list . fmap pretty . F.toList

-- | Pretty print a tuple.
tupled :: F.Foldable f => f Doc -> Doc
tupled = encloseWith lparen rparen comma . F.toList

-- | > tupled' xs == tupled (fmap pretty xs)
tupled' :: (F.Foldable f, Pretty a) =>  f a -> Doc
tupled' = tupled . fmap pretty . F.toList

-- | @set ds@ encloses @ds@ in braces and seperates them using commas.
set :: F.Foldable f => f Doc -> Doc
set = encloseWith lbrace rbrace comma . F.toList

-- | > set' xs == set (fmap pretty . S.toList $ S.fromList xs)
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
listing xs = table [( AlignLeft, (<> colon <> space) `fmap` is), (AlignLeft, ds)]
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
display d = displayS (renderPretty 0.9 120 d) ""

-- | Pretty print to stdout.
putPretty :: Pretty a => a -> IO ()
putPretty = putStrLn . display . pretty

