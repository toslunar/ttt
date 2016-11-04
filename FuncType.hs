{-# LANGUAGE Rank2Types #-}
module FuncType where

import Lens
import Pointed
import MyShow

import qualified Data.Map as M

-- (Pointed a) => a -> k
newtype Func k a = Func { getFunc :: M.Map k a }
  deriving (Eq, Show)

instance Pointed (Func k a) where
  point = Func M.empty


infixl 9 !
(!) :: (Ord k, Pointed a) => Func k a -> k -> a
f ! k = M.findWithDefault point k (getFunc f)

setValue :: (Ord k, Pointed a, Eq a) => k -> a -> Func k a -> Func k a
setValue k a
  | a == point  = Func . M.delete k . getFunc
  | otherwise  = Func . M.insert k a . getFunc

at :: (Ord k, Pointed a, Eq a) => k -> Lens' (Func k a) a
at n h f = fmap (\ x -> setValue n x f) (h (f ! n))


instance (MyShow k, MyShow a, Pointed a, Eq a) => MyShow (Func k a) where
  myShowsPrec d f = let
    mapstoPrec1 = 0
    mapsto = ": "  -- " |-> "
    showx (k, a) =
      myShowsPrec mapstoPrec1 k .
      showString mapsto .
      myShowsPrec mapstoPrec1 a
    showy =
      showChar '_' .
      showString mapsto .
      -- myShowsPrec 11 (point :: a)  <<< this does not work!  (why?)
      myShowsPrec mapstoPrec1 (let pt = point ; b = pt == snd (head l) in pt)  -- too bad
    l = filter ((/= point) . snd) . M.toList . getFunc $ f
    in showChar '{' .
      foldr (\ v r -> showx v . showString ", " . r) showy l .
      showChar '}'
