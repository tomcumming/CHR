module CS.Partial
  ( MatchCons (..),
    Var,
    Partial,
    PartialSubs,
    subs,
    combineSubs,
    free,
    unsafeCoerceVar,
  )
where

import CS.Unify (Match (..), Unify (..))
import Control.Monad.Free (Free (..))
import Control.Monad.Trans.Free qualified as FF
import Data.Bifunctor (bimap)
import Data.Foldable (fold, foldlM, toList)
import Data.Functor.Foldable (cata)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

-- | Match constructors
class (Foldable f) => MatchCons f where
  matchCons :: f a -> f b -> Bool

data Var f = Var Int
  deriving (Eq, Ord)

instance Enum (Var f) where
  toEnum = Var
  fromEnum (Var x) = x

instance Show (Var f) where show (Var x) = show x

unsafeCoerceVar :: Var a -> Var b
unsafeCoerceVar (Var x) = Var x

type Partial f = Free f (Var f)

type PartialSubs f = Map.Map (Var f) (Partial f)

subs :: (Functor f) => PartialSubs f -> Partial f -> Partial f
subs s = cata $ \case
  FF.Pure x -> fromMaybe (Pure x) $ s Map.!? x
  FF.Free f -> Free f

free :: (Functor f, Foldable f) => Partial f -> Set.Set (Var f)
free = cata $ \case
  FF.Pure x -> Set.singleton x
  FF.Free f -> fold f

trySolve ::
  (Functor f, Foldable f) =>
  Var f ->
  Partial f ->
  Maybe (PartialSubs f)
trySolve x p
  | Pure y <- p, x == y = Just mempty
  | x `Set.member` free p = Nothing
  | otherwise = Just $ Map.singleton x p

-- | Combine subs by applying s2 to s1
combineSubs :: (Functor f) => PartialSubs f -> PartialSubs f -> PartialSubs f
combineSubs s1 s2 = s2 <> (subs s2 <$> s1)

data Skol g f
  = Partial (g f)
  | Skol (Var g)
  deriving (Functor, Foldable)

skolemize :: (Functor f) => Partial f -> Skolemized f
skolemize =
  Skolemized
    . ( cata $ \case
          FF.Pure x -> Free $ Skol x
          FF.Free p -> Free $ Partial p
      )

liftPartial :: (Functor f) => Partial f -> Skolemized f
liftPartial =
  Skolemized
    . ( cata $ \case
          FF.Pure x -> Pure $ unsafeCoerceVar x
          FF.Free p -> Free $ Partial p
      )

unskol :: (Functor f) => Partial (Skol f) -> Partial f
unskol = cata $ \case
  FF.Pure x -> pure $ unsafeCoerceVar x
  FF.Free (Skol x) -> Pure x
  FF.Free (Partial p) -> Free p

unskolSubs :: (Functor f) => PartialSubs (Skol f) -> PartialSubs f
unskolSubs =
  fmap unskol
    . Map.mapKeysMonotonic unsafeCoerceVar
    . Map.filterWithKey notRefl
  where
    notRefl :: Var (Skol f) -> Partial (Skol f) -> Bool
    notRefl x = \case
      Free (Skol y) -> unsafeCoerceVar x /= y
      _ -> True

newtype Skolemized f = Skolemized {unSkolemized :: Partial (Skol f)}

instance (Functor f, MatchCons f) => Unify (Skolemized f) where
  type Subs (Skolemized f) = PartialSubs (Skol f)

  unify = curry $ (. unwrap) $ \case
    (Pure x, p) -> trySolve x p
    (p, Pure x) -> trySolve x p
    (Free (Skol s1), Free (Skol s2)) | s1 == s2 -> Just mempty
    (Free (Partial p1), Free (Partial p2))
      | matchCons p1 p2 -> foldlM go mempty $ zip (toList p1) (toList p2)
    _ -> Nothing
    where
      unwrap = bimap unSkolemized unSkolemized

      go ::
        PartialSubs (Skol f) ->
        (Partial (Skol f), Partial (Skol f)) ->
        Maybe (PartialSubs (Skol f))
      go s1 (p1, p2) = do
        s2 <- unify (Skolemized $ subs s1 p1) (Skolemized $ subs s1 p2)
        pure $ combineSubs s1 s2

instance (Functor f, MatchCons f) => Unify (Partial f) where
  type Subs (Partial f) = PartialSubs f
  unify p1 p2 = unskolSubs <$> unify (liftPartial p1) (liftPartial p2)

instance (Functor f, MatchCons f) => Match (Partial f) where
  match p1 p2 = unskolSubs <$> unify (liftPartial p1) (skolemize p2)
