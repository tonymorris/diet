module Data.Set.Diet.Tests
  (
    main
  , test
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Set.Diet
import Data.Set.Diet.Data

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
    testGroup "M"
      [
        testProperty "Right Identity" prop_right_identity
      ]

prop_right_identity ::
  Int
  -> Bool
prop_right_identity n =
  n + 0 == n

