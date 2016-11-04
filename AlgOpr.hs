{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module AlgOpr where

-- import Control.Monad.List
import Nondeterministic

import Control.Monad.State
import qualified Data.Map as M

-- i-ary algebraic operation on monad m
type AlgOpr m i = forall a b . (i -> a -> m b) -> a -> m b


-- nondeterministic choice: binary operation
oplusOpr :: (MonadNondet m) => AlgOpr m Bool
oplusOpr f a = chooseNondet [f bit a | bit <- [False, True]]

-- oplus :: (Monad m) => AlgOpr (NonDetT m) Bool
-- oplus f a = mplus (f False a) (f True a)
-- oplus f a = ListT $ do
--   xs <- runListT (f False a)
--   ys <- runListT (f True a)
--   return $ xs ++ ys


lookupOpr :: (MonadState (M.Map l v) m, Ord l) => l -> AlgOpr m v
lookupOpr loc f a = do
  s <- get
  f (s M.! loc) a

updateOpr :: (MonadState (M.Map l v) m, Ord l) => l -> v -> AlgOpr m ()
updateOpr loc val f a = do
  modify $ M.insert loc val
  f () a
