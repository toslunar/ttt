{-# LANGUAGE NoMonomorphismRestriction, Rank2Types #-}
module Transducer where

import Nat
import Lens
import FuncType
import Pointed
import Arrow

import Control.Monad.State

-- (Monad m) => (a, x) -> m (b, x)
newtype Td m x a b = Td { getTd :: a -> StateT x m b }
-- abbreviate Transducer as Td

-- run transducer t with input a
runTd :: (Monad m, Pointed x) => Td m x a b -> a -> m (b, x)
runTd t a = runStateT (getTd t a) point

-- J: lifts a Kleisli morphism
jTd :: (Monad m) => (a -> m b) -> Td m () a b
jTd f = Td $ \ a -> lift (f a)

-- J0: lifts a morphism
j0Td :: (Monad m) => (a -> b) -> Td m () a b
j0Td f = jTd (return . f)

-- identity
idTd :: (Monad m) => Td m () a a
idTd = j0Td id

-- composition
compTd :: (Monad m) => Td m y b c -> Td m x a b -> Td m (x, y) a c
compTd t s = Td $
  getTd (liftFst s) >=> getTd (liftSnd t)
{-
compTd t s = Td $ \ a -> StateT $ \ (x0, y0) -> do
  (b, x1) <- runStateT (getTd s a) x0
  (c, y1) <- runStateT (getTd t b) y0
  return (c, (x1 ,y1))
-}

-- monoidal product (i.e. binary sum)
mprodTd :: (Monad m) =>
  Td m x a b -> Td m y c d -> Td m (x, y) (Either a c) (Either b d)
mprodTd s t = Td $ either
  (getTd $ mapTd id Left  (liftFst s))
  (getTd $ mapTd id Right (liftSnd t))
{-
mprodTd s t = Td $ either
  (mapStateT (liftM (Left  *** id)) . getTd (liftFst s))
  (mapStateT (liftM (Right *** id)) . getTd (liftSnd t))
-}

-- Functor F = N * _  (i.e. countable sum)
functorBoxTd :: (Monad m, Pointed x, Eq x) =>
  Td m x a b -> Td m (Func Nat x) (Nat, a) (Nat, b)
functorBoxTd t = Td $ \ (n, a) ->
    ($ a) . getTd $ mapTd id (\ b -> (n, b)) (liftBy (at n) t)

{-
functorBoxTd t = Td $ \ (n, a) -> StateT $ \ f -> do
  (b, x) <- runStateT (getTd t a) (f ! n)
  return ((n, b), setValue n x f)
-}
{- The definition below is correct, but not "Show (Nat -> x)".
functorBoxTd :: (Monad m) => Td m x a b -> Td m (Nat -> x) (Nat, a) (Nat, b)
functorBoxTd t = Td $ \ (n, a) -> StateT $ \ f -> do
  (b, x) <- runStateT (getTd t a) (f n)
  return ((n, b), \ m -> if m == n then x else f m)
-}

-- trace
traceTd :: (Monad m) => Td m x (Either a c) (Either b c) -> Td m x a b
traceTd t = Td (loop . Left)  where
  loop ac = do
    bc <- getTd t ac
    case bc of
      Left b -> return b
      Right c -> loop (Right c)


{- for implimentation -}

liftBy :: (Monad m) => Lens' y x -> Td m x a b -> Td m y a b
liftBy l t = Td $ \ a -> StateT $ \ y -> do
  (b, x') <- runStateT (getTd t a) (y ^. l)
  return (b, l .~ x' $ y)
-- liftBy :: (Monad m) => (y -> (x, x -> y)) -> Td m x a b -> Td m y a b
-- liftBy f t = Td $ \ a -> StateT $ \ y ->
--   let (x, g) = f y
--   in do
--     (b, x') <- runStateT (getTd t a) x
--     return (b, g x')

liftFst :: (Monad m) => Td m x a b -> Td m (x, y) a b
liftFst = liftBy _1  -- $ \ (x, y) -> (x, \ x' -> (x', y))
-- liftFst t = Td $ \ a -> StateT $ \ (x, y) -> do
--   (b, x') <- runStateT (getTd t a) x
--   return (b, (x', y))

liftSnd :: (Monad m) => Td m y a b -> Td m (x, y) a b
liftSnd = liftBy _2  -- $ \ (x, y) -> (y, \ y' -> (x, y'))
-- liftSnd t = Td $ \ a -> StateT $ \ (x, y) -> do
--   (b, y') <- runStateT (getTd t a) y
--   return (b, (x, y'))

unliftUnit :: (Monad m) => Td m ((), y) a b -> Td m y a b
unliftUnit = liftBy l  where
  l h y = fmap (\ ((), y') -> y') (h ((), y))

unliftUnit' :: (Monad m) => Td m (x, ()) a b -> Td m x a b
unliftUnit' = liftBy l  where
  l h x = fmap (\ (x', ()) -> x') (h (x, ()))

mapTd :: (Monad m) => (a1 -> a) -> (b -> b1) -> Td m x a b -> Td m x a1 b1
mapTd f g t = Td $ mapStateT (liftM (g *** id)) . getTd t . f

mapTd' :: (Monad m) => (a1 -> m a) -> (b -> m b1) -> Td m x a b -> Td m x a1 b1
mapTd' before after t =
  unliftUnit' $ compTd (jTd after) $ unliftUnit $ compTd t (jTd before)

mprodTd' kl t = unliftUnit $ mprodTd (jTd kl) t

