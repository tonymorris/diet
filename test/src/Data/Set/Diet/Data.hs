{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Set.Diet.Data where

import Data.Set.Diet
import Test.QuickCheck

instance (Ord a, Enum a, Arbitrary a) => Arbitrary (Diet a) where
  arbitrary =
    let fromList' ::
          (Ord x, Enum x) =>
          [x]
          -> Diet x
        fromList' = fromList
    in fmap fromList' arbitrary