module Data.Fortran.Unformatted (toUnformatted , fromUnformatted) where
    
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy as BS
import Data.ByteString.Builder


fromUnformatted :: ByteString -> ByteString
fromUnformatted = toLazyByteString . mconcat . blocks . skipBOF
  where
    blocks :: ByteString -> [Builder]
    blocks s = let (block',rest') = BS.splitAt (min 128 blockSize) (BS.tail s)
                   header         = BS.head s
                   atEof          = blockSize == 130
                   blockSize      = fromIntegral header
                   block          = if header == trailer
                                    then lazyByteString block'
                                    else error "fromUnformatted: Bad trailer"
                   (trailer,rest) = uncons' rest'
               in if atEof then [] else block : blocks rest
    skipBOF s = if fst (uncons' s) == 75
                then snd (uncons' s)
                else error "fromUnformatted: Invalid file. BOF /= 75"
    uncons' = fromMaybe (error "fromUnformatted: unexpected EOF") . BS.uncons


toUnformatted :: ByteString -> ByteString
toUnformatted = undefined
