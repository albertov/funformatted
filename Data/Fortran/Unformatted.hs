{-# LANGUAGE RankNTypes #-}
module Data.Fortran.Unformatted (
    fromUnformatted
  , fromUnformatted2
  , unformatted
) where

import Data.Monoid (mconcat)
import Data.Fortran.Unformatted.Parser (unformatted)
import qualified Data.Fortran.Unformatted.Unsafe as Unsafe
import Pipes (Producer')
import Pipes.ByteString (fromLazy)
import Pipes.Attoparsec (parse)
import Pipes.Parse (evalStateT)
import Data.ByteString.Lazy as LBS (ByteString)
import Data.ByteString as BS (ByteString)


fromUnformatted :: Monad m => LBS.ByteString -> m BS.ByteString
fromUnformatted s = do result <- evalStateT (parse unformatted) . fromLazy $ s
                       case result of
                         Left e   -> fail $ show e
                         Right bs -> return $ mconcat bs

fromUnformatted2 :: Monad m => LBS.ByteString -> m (Either String BS.ByteString)
fromUnformatted2 s = do result <- evalStateT (parse unformatted) . fromLazy $ s
                        case result of
                          Left e   -> return $ Left $ show e
                          Right bs -> return $ Right $ mconcat bs
