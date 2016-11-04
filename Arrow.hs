module Arrow where

{- cf. Control.Arrow -}

infixr 3 ***
infixr 2 +++

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
f *** g = \ (x, z) -> (f x, g z)

(+++) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
f +++ g = either (Left . f) (Right . g)
