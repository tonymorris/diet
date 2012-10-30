module Data.Set.Diet.Tests
  (
    main
  , test
  ) where

import Test.QuickCheck.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Set.Diet
import Data.Set.Diet.Data()

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
    testGroup "Diet"
      [
        testProperty "Point has equal min and max" prop_point1
      , testProperty "Point is pointed" prop_point_is_pointed
      , testProperty "Interval min is less than max" prop_interval_min_lt_max
      , testProperty "Map maintains identity" prop_map_id
      , testProperty "Map maintains composition" prop_map_composes
      , testProperty "member inequivalent to notMember" prop_member_not_member
      ]

prop_point1 ::
  Int
  -> Bool
prop_point1 n =
  all (\f -> f (point n) == n) [intervalMin, intervalMax]

prop_point_is_pointed ::
  Int
  -> Bool
prop_point_is_pointed =
  isPointed . point

prop_interval_min_lt_max ::
  Int
  -> Int
  -> Bool
prop_interval_min_lt_max a1 a2 =
  let i = interval a1 a2
  in intervalMin i <= intervalMax i

prop_map_id ::
  Interval Int
  -> Bool
prop_map_id i =
  mapI id i == i

prop_map_composes ::
  Fun Int Int
  -> Fun Int Int
  -> Interval Int
  -> Bool
prop_map_composes f g i =
  let f' = apply f
      g'= apply g
  in mapI (f' . g') i == mapI f' (mapI g' i)

prop_member_not_member ::
  Int
  -> Diet Int
  -> Bool
prop_member_not_member d x =
  member d x /= notMember d x