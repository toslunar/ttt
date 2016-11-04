module NatEncoded
  (
    Nat
  , uu, vv, phi, psi
  ) where


data Nat =
    Nat Integer
  | Gauche Nat
  | Droit Nat
  | Pair Nat Nat
  deriving (Eq, Ord)
-- "Ord Nat" is only for "Data.Map.Map Nat x"

instance Show Nat where
  showsPrec d (Nat n) = showsPrec d n

  showsPrec d (Gauche n) =
       showString "g" . showsPrec 11 n

  showsPrec d (Droit n) =
       showString "d" . showsPrec 11 n

  showsPrec d (Pair n m) =
       showChar '<' .
       shows n .
       showChar ',' .
       shows m .
       showChar '>'

instance Num Nat where
  fromInteger n
    | n >= 0  = Nat n
    | otherwise  = error "fromInteger Nat: negative"

  (Nat n) + (Nat m) = Nat (n + m)
  _ + _ = error "(+) Nat: encoded"

  (Nat n) * (Nat m) = Nat (n * m)
  _ * _ = error "(*) Nat: encoded"

  abs = id

  signum (Nat 0) = (Nat 0)
  signum (Nat _) = (Nat 1)
  signum _ = error "signum Nat: encoded"

  (Nat n) - (Nat m) = let k = n - m in if k >= 0
    then Nat k
    else error "(-) Nat: negative"
  _ - _ = error "(-) Nat: encoded"


uu :: (Nat, Nat) -> Nat
uu (n, m) = Pair n m

vv :: Nat -> (Nat, Nat)
vv (Pair n m) = (n, m)
vv x = error $ "vv: input = " ++ show x

phi :: Either Nat Nat -> Nat
phi (Left n) = Gauche n
phi (Right n) = Droit n

psi :: Nat -> Either Nat Nat
psi (Gauche n) = Left n
psi (Droit n) = Right n
psi x = error $ "psi: input = " ++ show x
