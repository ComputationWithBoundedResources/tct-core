{- |


This module provides the main function of TcT.

-}

import Tct (tctl, applyMode, void)


main :: IO ()
main = tctl $ applyMode void

