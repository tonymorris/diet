module Main where

import qualified Data.Set.Diet.Tests
import Test.Framework

main ::
  IO ()
main = 
  defaultMain tests 

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        Data.Set.Diet.Tests.test
      ]
  ]

