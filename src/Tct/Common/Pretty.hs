-- | This module re-exports "Text.PrettyPrint.ANSI.Leijen"
-- (<http://hackage.haskell.org/package/ansi-wl-pprint>)
-- and provides supplementary pretty-printing functions.
module Tct.Common.Pretty
  (
  module Text.PrettyPrint.ANSI.Leijen
  , Align (..)
  , table
  , paragraph
  , display
  ) where


import Data.List                    (transpose)
import Text.PrettyPrint.ANSI.Leijen


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
                      len = maximum $ concat [ map length c | c <- cs']]
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

-- | Constructs a paragraph, respecting newline characters.
paragraph :: String -> Doc
paragraph s = vcat [fillSep [text w | w <- words l] | l <- lines s]

-- | Default 'Doc' rendering.
display :: Doc -> String
display d = displayS (renderPretty 0.9 1000 d) ""

