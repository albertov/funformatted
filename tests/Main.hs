import Test.Hspec

import qualified TestUnformatted (spec)

allSpecs :: [Spec]
allSpecs = [ TestUnformatted.spec ]

main :: IO ()
main = hspec $ foldr (>>) (return ()) allSpecs
