module Pointed where

-- type with canonical element
class Pointed a where
  point :: a

instance Pointed () where
  point = ()

instance (Pointed a, Pointed b) => Pointed (a, b) where
  point = (point, point)

{-
instance (Pointed a) => Pointed (b -> a) where
  point = const point
-}

-- Warning: To implement algebraic operations, do not "point = Just point"
instance Pointed (Maybe a) where
  point = Nothing
