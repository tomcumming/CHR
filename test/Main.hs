module Main (main) where

import CS.Partial (MatchCons (..), Partial)
import CS.Unify (Match (..))
import Control.Monad.Free (Free (..))
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data Inst a
  = ICon String
  | IAp a a
  deriving (Functor, Foldable, Show, Generic1)
  deriving (Show1) via FunctorClassesDefault Inst

instance MatchCons Inst where
  matchCons = curry $ \case
    (ICon c1, ICon c2) -> c1 == c2
    (IAp {}, IAp {}) -> True
    _ -> False

data Ty a
  = TVar String
  | TAp a a
  -- TODO Forall (debruijn)
  deriving (Functor, Foldable, Show, Generic1)
  deriving (Show1) via FunctorClassesDefault Ty

maybeHead, someVar :: Partial Inst
maybeHead =
  Free $
    IAp
      (Free $ ICon "Maybe")
      (Pure $ toEnum 1)
someVar = Pure $ toEnum 3

main :: IO ()
main = do
  putStrLn "Should be nothing..."
  print $ match maybeHead someVar
  putStrLn "Should be just..."
  print $ match someVar maybeHead
