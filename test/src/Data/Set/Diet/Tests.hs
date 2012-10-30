module Data.Set.Diet.Tests
  (
    main
  , test
  ) where

import Prelude hiding (all)
import Test.QuickCheck.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Foldable(all)
import Data.List(nub, sort)
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
        testProperty "Point has equal min and max" prop_point
      , testProperty "Point is pointed" prop_point_is_pointed
      , testProperty "Interval min is less than max" prop_interval_min_lt_max
      , testProperty "mergeI commutes" prop_mergei_commutes
      , testProperty "mergeI produces proper minimum" prop_mergei_min
      , testProperty "mergeI produces proper maximum" prop_mergei_max
      , testProperty "mapI maintains identity" prop_mapi_id
      , testProperty "mapI maintains composition" prop_mapi_composes
      , testProperty "member inequivalent to notMember" prop_member_not_member
      , testProperty "insert is a member" prop_insert_member
      , testProperty "delete is not a member" prop_delete_not_member
      , testProperty "empty has no members" prop_empty_no_members
      , testProperty "single has member" prop_single_one_member1
      , testProperty "single has one and only member" prop_single_one_member2
      , testProperty "size is greater than zero" prop_size_positive
      , testProperty "toList . fromList is inverse (with nub ignoring order)" prop_toList_fromList
      , testProperty "toList length equal to size" prop_toList_size
      , testProperty "toList has all members" prop_toList_members1
      , testProperty "toList has all and only all members" prop_toList_members2
      , testProperty "fromList has all members" prop_fromList_members1
      , testProperty "fromList has all and only all members" prop_fromList_members2
      , testProperty "fromList length equal to size" prop_fromList_size
      , testProperty "mapD maintains identity" prop_mapd_id
      , testProperty "mapD maintains composition" prop_mapd_composes
  ]

prop_point ::
  Int
  -> Bool
prop_point n =
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

prop_mergei_min ::
  Interval Int
  -> Interval Int
  -> Bool
prop_mergei_min i1 i2 =
  all (\i -> intervalMin i == min (intervalMin i1) (intervalMin i2)) (mergeI i1 i2)

prop_mergei_max ::
  Interval Int
  -> Interval Int
  -> Bool
prop_mergei_max i1 i2 =
  all (\i -> intervalMax i == max (intervalMax i1) (intervalMax i2)) (mergeI i1 i2)

prop_mergei_commutes ::
  Interval Int
  -> Interval Int
  -> Bool
prop_mergei_commutes i1 i2 =
  mergeI i1 i2 == mergeI i2 i1

prop_mapi_id ::
  Interval Int
  -> Bool
prop_mapi_id i =
  mapI id i == i

prop_mapi_composes ::
  Fun Int Int
  -> Fun Int Int
  -> Interval Int
  -> Bool
prop_mapi_composes f g i =
  let f' = apply f
      g'= apply g
  in mapI (f' . g') i == mapI f' (mapI g' i)

prop_member_not_member ::
  Int
  -> Diet Int
  -> Bool
prop_member_not_member x d =
  member x d /= notMember x d

prop_insert_member ::
  Int
  -> Diet Int
  -> Bool
prop_insert_member x d =
  member x (insert x d)

prop_delete_not_member ::
  Int
  -> Diet Int
  -> Bool
prop_delete_not_member x d =
  notMember x (delete x d)

prop_empty_no_members ::
  Int
  -> Bool
prop_empty_no_members x =
  notMember x empty

prop_single_one_member1 ::
  Int
  -> Bool
prop_single_one_member1 x =
  member x (single x)

prop_single_one_member2 ::
  Int
  -> Int
  -> Bool
prop_single_one_member2 x n =
  x == n || notMember n (single x)

prop_size_positive ::
  Diet Int
  -> Bool
prop_size_positive d =
  size d >= 0

prop_toList_fromList ::
  [Int]
  -> Bool
prop_toList_fromList x =
  sort (toList (fromList x)) == sort (nub x)

prop_toList_size ::
  Diet Int
  -> Bool
prop_toList_size x =
  length (toList x) == size x

prop_toList_members1 ::
  Diet Int
  -> Bool
prop_toList_members1 x =
  all (flip member x) (toList x)

prop_toList_members2 ::
  Diet Int
  -> Int
  -> Bool
prop_toList_members2 x n =
  elem n (toList x) || notMember n x

prop_fromList_members1 ::
  [Int]
  -> Bool
prop_fromList_members1 x =
  all (\n -> member n (fromList x)) x

prop_fromList_members2 ::
  [Int]
  -> Int
  -> Bool
prop_fromList_members2 x n =
  member n (fromList x) || notElem n x

prop_fromList_size ::
  [Int]
  -> Bool
prop_fromList_size x =
  size (fromList x) == length (nub x)

prop_mapd_id ::
  Diet Int
  -> Bool
prop_mapd_id i =
  mapD id i == i

prop_mapd_composes ::
  Fun Int Int
  -> Fun Int Int
  -> Diet Int
  -> Bool
prop_mapd_composes f g i =
  let f' = apply f
      g'= apply g
  in mapD (f' . g') i == mapD f' (mapD g' i)
