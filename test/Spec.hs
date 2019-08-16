import Test.Tasty

import Sodium.Init (sodiumInit)

import CloudTests
import SodiumTests

main :: IO ()
main = do
  sodiumInit
  defaultMain $ testGroup "all" [cloudTests, sodiumTests]
