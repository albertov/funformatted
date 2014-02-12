module Data.Fortran.Unformatted.Parser (unformatted) where

import Data.ByteString (ByteString)
import Data.Attoparsec as P
import Control.Applicative

unformatted :: Parser [ByteString]
unformatted = (bof <?> "Bad BOF") *> (many1 block <?> "Empty file")
  where
    bof = word8 75
    block = do
        header <- anyWord8
        if header /= 130
            then do block' <- P.take $ min 128 $ fromIntegral header
                    _ <- word8 header <?> "Bad trailer"
                    return block'
            else fail "eof"
