{-# LANGUAGE ScopedTypeVariables #-}
module TestUnformatted (spec) where

import Test.Hspec
import Control.Exception (SomeException, try)
import Data.Fortran.Unformatted (fromUnformatted, unformattedP)
import System.IO (IOMode(ReadMode), openFile)
import Data.ByteString.Lazy  as LBS (hGetContents, length)
import Pipes.ByteString (toLazyM)

spec :: Spec
spec = describe "Data.Fortran.Unformatted" $ do

  let goodFile = "tests/good.unf"
      badFile = "tests/bad.unf"
      loadBS f = openFile f ReadMode >>= hGetContents

  describe "fromUnformatted" $ do
    it "can read a good file" $ do
        bs <- loadBS goodFile
        let unf = fromUnformatted bs
        LBS.length unf `shouldBe` 192204
        LBS.length unf < LBS.length bs `shouldBe` True

  describe "unformattedP" $ do
    let fromUnformatted' = toLazyM . unformattedP
    it "can read a good file" $ do
        bs <- loadBS goodFile
        unf <- fromUnformatted' bs
        LBS.length unf `shouldBe` 192204
        LBS.length unf < LBS.length bs `shouldBe` True

    it "handles errors" $ do
        bs <- loadBS badFile
        unf <- try $ fromUnformatted' bs
        case unf of
          Right _                    -> fail "should be Left"
          Left (_ :: SomeException)  -> return ()
