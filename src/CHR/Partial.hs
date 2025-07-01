module CHR.Partial
  ( MatchCons (..),
    Var,
    Partial,
    PartialSubs,
    partialSubs,
    combineSubs,
    free,
    simpleUnify,
    simpleMatch,
    unsafeCoerceVar,
  )
where

import CHR.Unify (Unify (..))
import Control.Monad (guard)
import Control.Monad.Free (Free (..))
import Control.Monad.Trans.Free qualified as FF
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

partialSubs :: (Functor f) => PartialSubs f -> Partial f -> Partial f
partialSubs s = cata $ \case
  FF.Pure x -> fromMaybe (Pure x) $ s Map.!? x
  FF.Free f -> Free f

free :: (Functor f, Foldable f) => Partial f -> Set.Set (Var f)
free = cata $ \case
  FF.Pure x -> Set.singleton x
  FF.Free f -> fold f

trySolve :: (Functor f, Foldable f) => Var f -> Partial f -> Maybe (PartialSubs f)
trySolve x p
  | Pure y <- p, x == y = undefined
  | x `Set.member` free p = Nothing
  | otherwise = Just $ Map.singleton x p

-- | Combine subs by applying s2 to s1
combineSubs :: (Functor f) => PartialSubs f -> PartialSubs f -> PartialSubs f
combineSubs s1 s2 = s2 <> (partialSubs s2 <$> s1)

simpleUnify ::
  forall f.
  (Functor f, MatchCons f) =>
  Partial f ->
  Partial f ->
  Maybe (PartialSubs f)
simpleUnify = curry $ \case
  (Pure x, p) -> trySolve x p
  (p, Pure x) -> trySolve x p
  (Free p1, Free p2) -> do
    guard $ matchCons p1 p2
    foldlM go mempty $ zip (toList p1) (toList p2)
  where
    go :: PartialSubs f -> (Partial f, Partial f) -> Maybe (PartialSubs f)
    go s1 (p1, p2) = do
      s2 <- simpleUnify (partialSubs s1 p1) (partialSubs s1 p2)
      pure $ combineSubs s1 s2

data Skol g f
  = Partial (g f)
  | Skol (Var g)
  deriving (Functor, Foldable)

instance (MatchCons g) => MatchCons (Skol g) where
  matchCons = curry $ \case
    (Skol x, Skol y) -> x == y
    (Partial p1, Partial p2) -> matchCons p1 p2
    _ -> False

instance (Functor g, MatchCons g) => Unify (Partial (Skol g)) where
  type Subs (Partial (Skol g)) = PartialSubs (Skol g)

  unify p1 p2 = simpleUnify p1 p2

skolemize :: (Functor f) => Partial f -> Partial (Skol f)
skolemize = cata $ \case
  FF.Pure x -> Free $ Skol x
  FF.Free p -> Free $ Partial p

liftPartial :: (Functor f) => Partial f -> Partial (Skol f)
liftPartial = cata $ \case
  FF.Pure x -> Pure $ unsafeCoerceVar x
  FF.Free p -> Free $ Partial p

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

simpleMatch ::
  forall f.
  (Functor f, MatchCons f, Unify (Partial f)) =>
  Partial f ->
  Partial f ->
  Maybe (PartialSubs f)
simpleMatch p1 p2 = unskolSubs <$> unify (liftPartial p1) (skolemize p2)
