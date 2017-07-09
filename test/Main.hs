import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Lib

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

-- TODO: some tests would be good for future avi to write.
tests :: [TestTree]
tests = []
