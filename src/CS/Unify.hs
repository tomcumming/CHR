module CS.Unify
  ( Unify (..),
    Match (..),
  )
where

class Unify a where
  type Subs a

  unify :: a -> a -> Maybe (Subs a)

-- | One-way unification
class (Unify a) => Match a where
  -- | Find the substitution to turn first argument into second.
  match :: a -> a -> Maybe (Subs a)
