{- |
Module      :  Tct.Main
Copyright   :  (c) Martin Avanzini <martin.avanzini@uibk.ac.at>,
               Georg Moser <georg.moser@uibk.ac.at>,
               Andreas Schnabl <andreas.schnabl@uibk.ac.at>,
License     :  LGPL (see COPYING)

Maintainer  :  Martin Avanzini <martin.avanzini@uibk.ac.at>
Stability   :  unstable
Portability :  unportable


This module provides the main function of TcT.

-}




import Data.Void

import Tct


main :: IO ()
main = tctl $ defaultTctConfig (undefined :: Void)

