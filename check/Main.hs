module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Cute.String

main :: IO ()
main = defaultMain $ testGroup ""
  [ testCase "" $ assertEqual "" "a2c" (("a"…"c") 2)
  ]
