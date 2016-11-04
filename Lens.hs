{-# LANGUAGE Rank2Types, DeriveFunctor #-}
module Lens
  ( Lens, Lens'
  , over, set
  , views, view
  , (.~), (^.)
  , _1, _2
  ) where

{- cf. Control.Lens -}

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

newtype Id a = Id { getId :: a }
  deriving Functor
newtype Const a b = Const { getConst :: a }
  deriving Functor

over :: Lens s t a b -> (a -> b) -> s -> t
over l h = getId . l (Id . h)

set :: Lens s t a b -> b -> s -> t
set l x = over l (const x)

views :: Lens s t a b -> (a -> c) -> s -> c
views l h = getConst . l (Const . h)

view :: Lens s t a b -> s -> a
view l = views l id

infixr 4 .~
(.~) :: Lens s t a b -> b -> s -> t 
l .~ x = set l x

infixl 8 ^.
(^.) :: s -> Lens s t a b -> a
x ^. l = view l x


{- lenses -}

_1 :: Lens (a, c) (b, c) a b
_1 h (x, y) = fmap (\ x' -> (x', y)) (h x)

_2 :: Lens (c, a) (c, b) a b
_2 h (x, y) = fmap (\ y' -> (x, y')) (h y)
