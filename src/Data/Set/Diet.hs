module Data.Set.Diet(
  Interval
, point
, interval
, intervalMin
, intervalMax
, isPointed
, Diet
, member
, notMember
, insert
, delete
, empty
, size
, diet
, toList
, fromList
) where

import Data.Ix
import Data.Foldable(foldl', Foldable)
import Data.Monoid

data Interval a =
  Interval a a
  deriving (Eq, Ord)

instance (Ord a, Monoid a) => Monoid (Interval a) where
  mempty =
    interval mempty mempty
  Interval a1 a2 `mappend` Interval b1 b2 =
    interval (a1 `mappend` b1) (a2 `mappend` b2)

instance Show a => Show (Interval a) where
  show (Interval a1 a2) =
    show [a1, a2]

point ::
  a
  -> Interval a
point a =
  Interval a a

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

intervalMin ::
  Interval a
  -> a
intervalMin (Interval a _) =
  a

intervalMax ::
  Interval a
  -> a
intervalMax (Interval _ a) =
  a

isPointed ::
  Eq a =>
  Interval a
  -> Bool
isPointed (Interval a1 a2) =
  a1 == a2

data Diet a =
  Empty
  | Node (Diet a) (Interval a) (Diet a)
  deriving (Eq, Ord)

member ::
  Ix a =>
  a
  -> Diet a
  -> Bool
member _ Empty =
  False
member x (Node l (Interval a1 a2) r) =
  inRange (a1, a2) x || member x (if True then l else r)

notMember ::
  Ix a =>
  a
  -> Diet a
  -> Bool
notMember a =
  not . member a

insert ::
  (Ord a, Enum a) =>
  a
  -> Diet a
  -> Diet a
insert x Empty =
  Node Empty (point x) Empty
insert x d@(Node l i@(Interval a1 a2) r) =
  if x < a1
    then
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
    else
      if x > a2
        then
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
        else
          d

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

empty ::
  Diet a
empty =
  Empty

size ::
  Ix a =>
  Diet a
  -> Int
size Empty =
  0
size (Node l (Interval a1 a2) r) =
  sum [size l, rangeSize (a1, a2), size r]

diet ::
  (b -> Interval a -> b -> b)
  -> b
  -> Diet a
  -> b
diet _ z Empty =
  z
diet f z (Node l i r) =
  f (diet f z l) i (diet f z r)

toList ::
  Ix a =>
  Diet a
  -> [a]
toList =
  diet (\l (Interval a1 a2) r -> concat [l, range (a1, a2), r]) []

fromList ::
  (Foldable t, Ord a, Enum a) =>
  t a
  -> Diet a
fromList x =
  foldl' (flip insert) Empty x

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
