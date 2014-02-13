module Main where

import Prelude hiding (getContents, putStr)
import Data.Fortran.Unformatted (fromUnformatted)
import Data.ByteString.Lazy (getContents, putStr)
import Control.Monad (liftM)

main :: IO ()
main = liftM fromUnformatted getContents >>= putStr
