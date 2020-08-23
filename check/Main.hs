module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Cute.String

main :: IO ()
main = defaultMain $ testGroup ""
  [ testCase "" $ assertEqual "" "abc" (("a"…"c") "b")
  , testCase "" $ assertEqual "" "a\"b\"c" (("a"…"c") (show "b"))
  , testCase "" $ assertEqual "" "a'b'c" (("a"…"c") 'b')
  ]
