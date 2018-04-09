module Data.List.Unique.Internal where

import Prelude

import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List as List
import Data.Ord (class Ord1, compare1)
import Data.Set (Set)

data List a = MkList (List.List a) (Set a)

unwrap :: forall a. List a -> List.List a
unwrap (MkList l _) = l

derive instance eqUnique :: Eq a => Eq (List a)
instance eq1Unique :: Eq1 List where
  eq1 (MkList l _) (MkList r _) = eq1 l r

derive instance ordUnique :: Ord a => Ord (List a)
instance ord1Unique :: Ord1 List where
  compare1 (MkList l _) (MkList r _) = compare1 l r

instance foldableUnique :: Foldable List where
  foldr f acc (MkList l _) = foldr f acc l
  foldl f acc (MkList l _) = foldl f acc l
  foldMap f (MkList l _) = foldMap f l
