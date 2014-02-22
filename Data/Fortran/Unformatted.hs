{-# LANGUAGE RankNTypes #-}
module Data.Fortran.Unformatted (
    toUnformatted
  , fromUnformatted
  , unformattedP
) where
    
import Prelude hiding (concat, splitAt)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (ByteString, uncons, splitAt, concat)
import qualified Data.ByteString as BS (ByteString)
import Pipes (Producer')
import Pipes.ByteString (fromLazy)

fromUnformatted :: ByteString -> ByteString
fromUnformatted = concat . blocks . skipBOF
  where
    blocks s = let (header,tail') = uncons' s
                   (block',rest') = splitAt (min 128 blockSize) tail'
                   atEof          = blockSize == 130
                   blockSize      = fromIntegral header
                   block          = if header == trailer
                                    then block'
                                    else error "fromUnformatted: Bad trailer"
                   (trailer,rest) = uncons' rest'
               in if atEof then [] else block `seq` block : blocks rest
    skipBOF s = let (bof,rest) = uncons' s
                in if bof == 75
                then rest
                else error "fromUnformatted: Invalid file. BOF /= 75"
    uncons' = fromMaybe (error "fromUnformatted: unexpected EOF") . uncons
{-# INLINEABLE fromUnformatted #-}

toUnformatted :: ByteString -> ByteString
toUnformatted = undefined

unformattedP :: Monad m => ByteString -> Producer' BS.ByteString m ()
unformattedP = fromLazy . fromUnformatted
