{-# LANGUAGE RankNTypes #-}
module Data.Fortran.Unformatted (
    fromUnformatted
  , unformattedP
) where

import Data.Fortran.Unformatted.Parser (fromUnformatted)
import qualified Data.Fortran.Unformatted.Unsafe as Unsafe
import Pipes (Producer')
import Pipes.ByteString (fromLazy)
import Data.ByteString.Lazy as LBS (ByteString)
import Data.ByteString as BS (ByteString)


unformattedP :: Monad m => LBS.ByteString -> Producer' BS.ByteString m ()
unformattedP = fromLazy . Unsafe.fromUnformatted
