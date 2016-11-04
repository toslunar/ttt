{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NatRaw
  (
    Nat
  , uu, vv, phi, psi
  ) where


newtype Nat = Nat { getNat :: Integer }
  deriving (Eq, Ord, Real, Enum, Integral)

instance Show Nat where
  showsPrec d (Nat n) = showsPrec d n

instance Num Nat where
  fromInteger n
    | n >= 0  = Nat n
    | otherwise  = error "fromInteger Nat: negative"

  (Nat n) + (Nat m) = Nat (n + m)

  (Nat n) * (Nat m) = Nat (n * m)

  abs = id

  signum (Nat 0) = (Nat 0)
  signum (Nat _) = (Nat 1)

  (Nat n) - (Nat m) = let k = n - m in if k >= 0
    then Nat k
    else error "(-) Nat: negative"

uu :: (Nat, Nat) -> Nat
uu (0, 0) = 0
uu (n, m) = let
  (n', rn) = n `divMod` 2
  (m', rm) = m `divMod` 2
  in rn + 2 * (rm + 2 * uu (n', m'))

vv :: Nat -> (Nat, Nat)
vv 0 = (0, 0)
vv k = let
  (l, rn) = k `divMod` 2
  (k', rm) = l `divMod` 2
  (n', m') = vv k'
  in (rn + 2 * n', rm + 2 * m')

{- this encoding is slow !!
uu :: (Nat, Nat) -> Nat
uu (l, r) =
  if r == 0
    then phi (Left l)
    else phi (Right $ uu (l, r - 1))

vv :: Nat -> (Nat, Nat)
vv n =
  case psi n of
    Left m -> (m, 0)
    Right m -> let (l, r) = vv m in (l, r + 1)
-}

phi :: Either Nat Nat -> Nat
phi (Left n) = 2 * n
phi (Right n) = 2 * n + 1

psi :: Nat -> Either Nat Nat
psi n = let
  (q,r) = n `divMod` 2
  in if r == 0
    then Left q
    else Right q
