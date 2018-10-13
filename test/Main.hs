import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Confetti

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

-- TODO: some tests would be good for future avi to write.
tests :: [TestTree]
tests = []
