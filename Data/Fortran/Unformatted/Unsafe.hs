module Data.Fortran.Unformatted.Unsafe (toUnformatted , fromUnformatted) where
    
import Prelude hiding (splitAt)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (ByteString, uncons, splitAt, toStrict)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)

fromUnformatted :: ByteString -> ByteString
fromUnformatted = toLazyByteString . blocks . skipBOF
  where
    blocks :: ByteString -> Builder
    blocks s = let (header,tail') = uncons' s
                   (block',rest') = splitAt (min 128 blockSize) tail'
                   atEof          = blockSize == 130
                   blockSize      = fromIntegral header
                   block          = if header == trailer
                                    then byteString $ toStrict block'
                                    else error "fromUnformatted: Bad trailer"
                   (trailer,rest) = uncons' rest'
               in if atEof then mempty else block `seq` block <> blocks rest
    skipBOF s = let (bof,rest) = uncons' s
                in if bof == 75
                then rest
                else error "fromUnformatted: Invalid file. BOF /= 75"
    uncons' = fromMaybe (error "fromUnformatted: unexpected EOF") . uncons
{-# INLINEABLE fromUnformatted #-}

toUnformatted :: ByteString -> ByteString
toUnformatted = undefined
