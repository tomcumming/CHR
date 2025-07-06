module CHR.IndexedSet
  ( Indexed (..),
    IndexedSet,
    asSet,
    insert,
    remove,
    popWithKey,
  )
where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

class (Ord a, Ord (Key a)) => Indexed a where
  type Key a
  keys :: a -> Set.Set (Key a)

data IndexedSet a = IndexedSet
  { isPrimary :: Set.Set a,
    isSecondary :: Map.Map (Key a) (Set.Set a)
  }

instance (Show a) => Show (IndexedSet a) where show = show . isPrimary

instance (Eq a) => Eq (IndexedSet a) where a == b = isPrimary a == isPrimary b

instance (Ord a) => Ord (IndexedSet a) where
  compare a b = compare (isPrimary a) (isPrimary b)

instance Foldable IndexedSet where foldMap f = foldMap f . isPrimary

asSet :: IndexedSet a -> Set.Set a
asSet = isPrimary

insert :: (Indexed a) => a -> IndexedSet a -> IndexedSet a
insert a is =
  IndexedSet
    { isPrimary = Set.insert a (isPrimary is),
      isSecondary =
        foldr
          (\k -> Map.insertWith (<>) k (Set.singleton a))
          (isSecondary is)
          (keys a)
    }

removeMany :: forall a. (Indexed a) => Set.Set a -> IndexedSet a -> IndexedSet a
removeMany as is =
  IndexedSet
    { isPrimary = Set.difference (isPrimary is) as,
      isSecondary =
        foldr
          (Map.update (pruneEmpty . (`Set.difference` as)))
          (isSecondary is)
          (foldMap keys as)
    }
  where
    pruneEmpty :: Set.Set a -> Maybe (Set.Set a)
    pruneEmpty s
      | Set.null s = Nothing
      | otherwise = Just s

remove :: forall a. (Indexed a) => a -> IndexedSet a -> IndexedSet a
remove a = removeMany (Set.singleton a)

popWithKey :: (Indexed a) => Key a -> IndexedSet a -> (Set.Set a, IndexedSet a)
popWithKey k is = (as, removeMany as is)
  where
    as = fromMaybe mempty $ isSecondary is Map.!? k
