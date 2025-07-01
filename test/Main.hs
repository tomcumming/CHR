module Main (main) where

import CHR.Partial
  ( MatchCons (..),
    Partial,
    PartialSubs,
    simpleMatch,
    simpleUnify,
  )
import CHR.Unify (Match (..), Unify (..))
import Control.Monad.Free (Free (..))
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data Ty a
  = Con String
  | Ap a a
  deriving (Functor, Foldable, Show, Generic1)
  deriving (Show1) via FunctorClassesDefault Ty

instance MatchCons Ty where
  matchCons = curry $ \case
    (Con c1, Con c2) -> c1 == c2
    (Ap {}, Ap {}) -> True
    _ -> False

instance Unify (Partial Ty) where
  type Subs (Partial Ty) = PartialSubs Ty
  unify = simpleUnify

instance Match (Partial Ty) where
  match = simpleMatch

maybeHead, someVar :: Partial Ty
maybeHead =
  Free $
    Ap
      (Free $ Con "Maybe")
      (Pure $ toEnum 1)
someVar = Pure $ toEnum 3

main :: IO ()
main = do
  putStrLn "Should be nothing..."
  print $ match maybeHead someVar
  putStrLn "Should be just..."
  print $ match someVar maybeHead
