module TestUnformatted (spec) where

import Test.Hspec
import Data.Fortran.Unformatted (fromUnformatted, unformattedP)
import qualified Data.Fortran.Unformatted.Unsafe as Unsafe (fromUnformatted)
import System.IO (IOMode(ReadMode), openFile)
import Data.ByteString.Lazy as BS (hGetContents, length)
import Pipes.ByteString (toLazyM)

spec :: Spec
spec = describe "Data.Fortran.Unformatted" $ do

  let goodFile = "tests/good.unf"
      loadBS   = openFile goodFile ReadMode >>= hGetContents

  describe "fromUnformatted (unsafe)" $ do
    it "can read a good unformatted file" $ do
        bs <- loadBS
        let unf = Unsafe.fromUnformatted bs
        BS.length unf `shouldBe` 192204
        BS.length unf < BS.length bs `shouldBe` True

  describe "fromUnformatted (monadic)" $ do
    it "can read a good unformatted file" $ do
        bs <- loadBS
        unf <- fromUnformatted bs
        BS.length unf `shouldBe` 192204
        BS.length unf < BS.length bs `shouldBe` True

  describe "unformattedP" $ do
    it "can read a good unformatted file" $ do
        bs <- loadBS
        unf <- toLazyM $ unformattedP bs
        BS.length unf `shouldBe` 192204
        BS.length unf < BS.length bs `shouldBe` True
