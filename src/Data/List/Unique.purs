module Data.List.Unique
  ( module Data.List.Unique
  , module ForReExport
  ) where

import Prelude hiding (map)

import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartialBecause)

import Data.List.Unique.Internal (List(..))
import Data.List.Unique.Internal (List, unwrap) as ForReExport

fromFoldable ::
  forall f a.
     Ord a
  => Functor f
  => Foldable f
  => f a -> Either (Set a) (List a)
fromFoldable fa =
  if duplicates == Map.empty
    then Right (MkList (List.fromFoldable fa) (Set.fromFoldable fa))
    else Left (Set.fromFoldable $ Map.keys duplicates)
  where
    duplicates =
      Map.filter (_ > 1) <<< Map.fromFoldableWith (+) $ (_ `Tuple` 1) <$> fa

toUnfoldable ::
     forall f a. Unfoldable f
  => List a
  -> f a
toUnfoldable (MkList l _) = List.toUnfoldable l

map ::
     forall a b. Ord b
  => (a -> b)
  -> List a
  -> Either (Set b) (List b)
map f (MkList l _) = fromFoldable $ f <$> l

unsafeMapBecause ::
     forall a b.
     Ord b
  => String
  -> (a -> b)
  -> List a
  -> List b
unsafeMapBecause s f l = unsafePartialBecause s $ fromRight $ map f l

traverse ::
  forall a b m.
     Applicative m
  => Ord b
  => (a -> m b)
  -> List a
  -> m (Either (Set b) (List b))
traverse f (MkList l _) = fromFoldable <$> Traversable.traverse f l

unsafeTraverseBecause ::
     forall a b m.
     Applicative m
  => Ord b
  => String
  -> (a -> m b)
  -> List a
  -> m (List b)
unsafeTraverseBecause s f l = fromRightNoted <$> traverse f l
  where
    fromRightNoted x = unsafePartialBecause s $ fromRight x

head :: forall b. Ord b => List b -> Maybe b
head = (<$>) (\r -> r.head) <<< uncons

tail :: forall b. Ord b => List b -> Maybe (List b)
tail = (<$>) (\r -> r.tail) <<< uncons

uncons ::
     forall b.
     Ord b
  => List b
  -> Maybe { head :: b, tail :: List b }
uncons (MkList (b:bs) s) = Just { head : b, tail : MkList bs (Set.delete b s) }
uncons (MkList List.Nil _) = Nothing

cons ::
     forall a.
     Ord a
  => { head :: a, tail :: List a }
  -> Maybe (List a)
cons { head, tail : MkList l s } =
  if head `Set.member` s
    then Nothing
    else Just $ MkList (head : l) (Set.insert head s)
