module Data.Fortran.Unformatted.Parser (
    unformatted
  , fromUnformatted
) where

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromChunks)
import Data.Attoparsec as P (Parser, word8, anyWord8, many1, take, (<?>))
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import Control.Applicative ((*>))
import Control.Monad (when)

unformatted :: Parser [BS.ByteString]
unformatted = (bof <?> "Bad BOF") *> (many1 block <?> "Empty file")
  where
    bof = word8 75
    block = do
        header <- anyWord8
        when (header == 130) (fail "eof")
        block' <- P.take $ min 128 $ fromIntegral header
        _ <- word8 header <?> "Bad trailer"
        return block'

fromUnformatted :: Monad m => LBS.ByteString -> m LBS.ByteString
fromUnformatted bs = case (eitherResult . parse unformatted) bs of
                        Left e  -> fail e
                        Right r -> return $ LBS.fromChunks r
