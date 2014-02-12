module TestUnformatted (spec) where

import Test.Hspec
import Data.Fortran.Unformatted
import System.IO (IOMode(ReadMode), openFile)
import Data.ByteString.Lazy as BS (hGetContents, length)

spec :: Spec
spec = describe "Data.Fortran.Unformatted" $ do
  describe "fromUnformatted" $ do
    let goodFile = "tests/good.unf"
    it "can read a good unformatted file" $ do
        bs <- openFile goodFile ReadMode >>= hGetContents
        let unf = fromUnformatted bs
        BS.length unf `shouldBe` 192204
        BS.length unf < BS.length bs `shouldBe` True
