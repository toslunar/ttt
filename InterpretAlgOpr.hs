{-# LANGUAGE FlexibleContexts #-}
module InterpretAlgOpr where

import InterpretPure
import Interpret
import TransducerAlgOpr
import Transducer
import AlgOpr
import Nondeterministic
import Nat
import Log
import Pointed
import MyShow

import Control.Monad
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Data.Maybe

import qualified Data.Map as M

oplusGoI str s t     = watchBounds str $ oplusGoILog s t
drfGoI str loc       = watchBounds str $ drfGoILog loc
asgGoI str loc val t = watchBounds str $ asgGoILog loc val t

-- oplusGoI :: (MonadNondet m, MonadWriter Log m, Pointed x, Pointed y, MyShow x, MyShow y) =>
--   String -> TdGoI m x -> TdGoI m y -> TdGoI m (Maybe (Either x y))
-- oplusGoI str s t  = watchBounds str $ oplusTd s t

oplusGoILog :: (MonadNondet m, MonadWriter Log m, Pointed x, Pointed y) =>
  TdGoI m x -> TdGoI m y -> TdGoI m (Maybe (Either x y))
oplusGoILog s t = commentAlgOpr "nondeterministic choice" $ oplusTd s t
{-
oplusGoILog s t = Td $ \ a -> do
  x0 <- get
  when (isNothing x0) $ tell $ Log [LogStr "nondeterministic choice"]
  getTd (oplusTd s t) a
-}

drfGoILog :: (MonadState (M.Map l Nat) m, MonadWriter Log m, Ord l, MyShow l) =>
  l -> TdGoI m (Maybe (Nat, ()))
drfGoILog loc = commentAlgOpr ("lookup(" ++ myShow loc ++ ")") $
  lookupTd loc (\ n -> constGoILog n)
{-
drfGoILog loc = Td $ \ a -> do
  x0 <- get
  when (isNothing x0) $ tell $ Log [LogStr $ "lookup_" ++ show loc]
  getTd (lookupTd loc (\ n -> constGoILog n)) a
-}

asgGoILog :: (MonadWriter Log m, MonadState (M.Map l v) m, Pointed x,
  Ord l, MyShow l, Show v)  =>
    l -> v -> TdGoI m x -> TdGoI m (Maybe ((), x))
asgGoILog loc val t =
  commentAlgOpr ("update(" ++ myShow loc ++ "," ++ show val ++")") $
    updateTd loc val (\ _ -> t)

-- say something at the first call of the algebraic operation
commentAlgOpr :: (MonadWriter Log m) =>
  String -> TdGoI m (Maybe x) -> TdGoI m (Maybe x)
commentAlgOpr str t = Td $ \ a -> do
  x0 <- get
  when (isNothing x0) $ tell $ Log [LogStr str]
  getTd t a
