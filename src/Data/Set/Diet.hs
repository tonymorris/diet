-- | Discrete Interval Encoding Tree described by Martin Erwig in /Diets for Fat Sets, January 1993/.
module Data.Set.Diet(
  Interval
, point
, interval
, intervalMin
, intervalMax
, mergeI
, isPointed
, mapI
, Diet
, member
, notMember
, insert
, delete
, empty
, single
, singleI
, size
, diet
, toList
, fromList
, mapD
) where

import Data.Ix
import Data.Foldable(foldl', Foldable)

-- | An interval with discrete values between.
data Interval a =
  Interval a a
  deriving (Eq, Ord)

instance (Eq a, Show a) => Show (Interval a) where
  show (Interval a1 a2) =
    '[' : show a1 ++ (if a1 == a2 then [] else '|' : show a2) ++ "]"

-- | An interval with the same minimum and maximum.
point ::
  a
  -> Interval a
point a =
  Interval a a

-- | Construct an interval ensuring that the minimum is less than or equal to maximum.
interval ::
  Ord a =>
  a
  -> a
  -> Interval a
interval a1 a2 =
  if a1 <= a2
    then
      Interval a1 a2
    else
      Interval a2 a1

-- | The minimum of the interval.
intervalMin ::
  Interval a
  -> a
intervalMin (Interval a _) =
  a

-- | The maximum of the interval.
intervalMax ::
  Interval a
  -> a
intervalMax (Interval _ a) =
  a

-- | Merge two intervals if they are overlapping or adjacent.
mergeI ::
  (Ord a, Enum a) =>
  Interval a
  -> Interval a
  -> Maybe (Interval a)
mergeI (Interval a1 a2) (Interval aa1 aa2) =
  if (a1 <= aa2 && succ a2 >= aa1) || (aa1 <= a2 && succ aa2 >= a1)
    then
      Just $ Interval (min a1 aa1) (max a2 aa2)
    else
      Nothing

-- | Returns whether or not the interval has the same minimum and maximum.
isPointed ::
  Eq a =>
  Interval a
  -> Bool
isPointed (Interval a1 a2) =
  a1 == a2

-- | Map a function across the minimum and maximum of the interval.
mapI ::
  Ord b =>
  (a -> b)
  -> Interval a
  -> Interval b
mapI f (Interval a1 a2) =
  interval (f a1) (f a2)

-- | A Discrete Interval Encoding Tree.
data Diet a =
  Empty
  | Node (Diet a) (Interval a) (Diet a)
  deriving (Eq, Ord)

instance (Eq a, Show a) => Show (Diet a) where
  showsPrec _ Empty =
    id
  showsPrec n (Node l i r) =
    showsPrec n l . shows i . showsPrec n r

-- | Test for membership in the interval tree.
member ::
  Ix a =>
  a
  -> Diet a
  -> Bool
member _ Empty =
  False
member x (Node l (Interval a1 a2) r) =
  inRange (a1, a2) x || member x (if x < a1 then l else r)

-- | Test for non-membership in the interval tree.
notMember ::
  Ix a =>
  a
  -> Diet a
  -> Bool
notMember a =
  not . member a

-- | Insert an element into the interval tree.
insert ::
  (Ord a, Enum a) =>
  a
  -> Diet a
  -> Diet a
insert x Empty =
  Node Empty (point x) Empty
insert x d@(Node l i@(Interval a1 a2) r)
  | x < a1 =
    if succ x == a1
      then
        let joinLeft md@(Node Empty _ _) =
              md
            joinLeft (Node ml mi@(Interval ma1 ma2) mr) =
              let (ml', Interval ml1 ml2) = splitMax ml
              in if succ ml2 == ma1
                   then
                     Node ml' (Interval ml1 ma2) mr
                   else
                     Node ml mi mr
            joinLeft Empty =
              error "Broken invariant @ Data.Set.Diet#joinLeft"
        in joinLeft (Node l (Interval x a2) r)
      else
        Node (insert x l) i r
  | x > a2 =
    if succ a2 == x
      then
        let splitMin (Node Empty mi mr) =
              (mr, mi)
            splitMin (Node ml mi mr) =
              let (md, mi') = splitMin ml
              in (Node md mi mr, mi')
            splitMin Empty =
              error "Broken invariant @ Data.Set.Diet#splitMin"
            joinRight jd@(Node _ _ Empty) =
              jd
            joinRight (Node jl ji@(Interval ja1 ja2) jr) =
              let (jr', Interval jr1 jr2) = splitMin jr
              in if succ ja2 == jr1
                   then
                     Node jl (Interval ja1 jr2) jr'
                   else
                     Node jl ji jr
            joinRight Empty =
              error "Broken invariant @ Data.Set.Diet#joinRight"
        in joinRight (Node l (Interval a1 x) r)
      else
        Node l i (insert x r)
  | otherwise =
    d

-- | Delete an element from the interval tree.
delete ::
  (Ord a, Enum a) =>
  a
  -> Diet a
  -> Diet a
delete _ Empty =
  Empty
delete x (Node l i@(Interval a1 a2) r)
  | x < a1 =
    Node (delete x l) i r
  | x > a2 =
    Node l i (delete x r)
  | x == a1 =
    let merge ml Empty =
          ml
        merge Empty mr =
          mr
        merge ml mr =
          let (ml', mi) = splitMax ml
          in Node ml' mi mr
    in if isPointed i
         then
           merge l r
         else
           Node l (Interval (succ a1) a2) r
  | x == a2 =
    Node l (Interval a1 (pred a2)) r
  | otherwise =
    Node l (Interval a1 (pred x)) (Node Empty (Interval (succ x) a2) r)

-- | Construct an interval tree with no elements.
empty ::
  Diet a
empty =
  Empty

-- | Construct an interval tree with a single element.
single ::
  a
  -> Diet a
single a =
  Node Empty (point a) Empty

-- | Construct an interval tree with a single interval.
singleI ::
  Interval a
  -> Diet a
singleI a =
  Node Empty a Empty

-- | Return the number of elements in the interval tree.
size ::
  Ix a =>
  Diet a
  -> Int
size Empty =
  0
size (Node l (Interval a1 a2) r) =
  sum [size l, rangeSize (a1, a2), size r]

-- | Fold on the interval tree.
diet ::
  (b -> Interval a -> b -> b)
  -> b
  -> Diet a
  -> b
diet _ z Empty =
  z
diet f z (Node l i r) =
  f (diet f z l) i (diet f z r)

-- | Return all elements of the interval tree as a list.
toList ::
  Ix a =>
  Diet a
  -> [a]
toList =
  diet (\l (Interval a1 a2) r -> concat [l, range (a1, a2), r]) []

-- | Construct an interval tree with the elements of the list.
fromList ::
  (Foldable t, Ord a, Enum a) =>
  t a
  -> Diet a
fromList =
  foldl' (flip insert) Empty

-- | Map a function across the interval tree.
mapD ::
  Ord b =>
  (a -> b)
  -> Diet a
  -> Diet b
mapD _ Empty =
  Empty
mapD f (Node l i r) =
  Node (mapD f l) (mapI f i) (mapD f r)

-- BEGIN not exported

splitMax ::
  Diet a
  -> (Diet a, Interval a)
splitMax (Node l i Empty) =
  (l, i)
splitMax (Node l i r) =
  let (d, i') = splitMax r
  in (Node l i d, i')
splitMax Empty =
  error "Broken invariant @ Data.Set.Diet#splitMax"

-- END not exported
