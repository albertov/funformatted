module TestUnformatted (spec) where

import Test.Hspec
import Data.Fortran.Unformatted (fromUnformatted, fromUnformatted2)
import qualified Data.Fortran.Unformatted.Unsafe as Unsafe (fromUnformatted)
import System.IO (IOMode(ReadMode), openFile)
import Data.ByteString.Lazy  as LBS (hGetContents, length)
import Data.ByteString.Char8 as BS (length)
import Pipes.ByteString (toLazyM)

spec :: Spec
spec = describe "Data.Fortran.Unformatted" $ do

  let goodFile = "tests/good.unf"
      badFile = "tests/bad.unf"
      loadBS f = openFile f ReadMode >>= hGetContents

  describe "fromUnformatted (unsafe)" $ do
    it "can read a good file" $ do
        bs <- loadBS goodFile
        let unf = Unsafe.fromUnformatted bs
        LBS.length unf `shouldBe` 192204
        LBS.length unf < LBS.length bs `shouldBe` True

  describe "fromUnformatted (monadic)" $ do
    it "can read a good file" $ do
        bs <- loadBS goodFile
        unf <- fromUnformatted bs
        BS.length unf `shouldBe` 192204
        BS.length unf < (fromIntegral $ LBS.length bs) `shouldBe` True

    it "handles errors" $ do
        bs <- loadBS badFile
        unf <- fromUnformatted2 bs
        case unf of
          Left e  -> e `shouldBe` "lala"
          Right _ -> fail "Should not have parsed"
        

