module OldInterpretPure where

import Interpret
import Transducer
import Nat
import FuncType
import Pointed
import Arrow


constGoI :: (Monad m) => Nat -> TdGoI m ()
constGoI i = traceTd $ mapTd before after $ unliftUnit $ mprodTd hTd (kTd i)
  where
    before :: Either Flow (Either Nat Nat) -> Either (Either Nat Nat) Nat
    before (Left (Nothing, n)) = Left (Left n)
    before (Left (Just var, n)) = error "Oops >_<"
    before (Right (Left n)) = Left (Right n)
    before (Right (Right n)) = Right n

    after :: Either (Either Nat Nat) Nat -> Either Flow (Either Nat Nat)
    after (Left (Left n)) = Left (Nothing, n)
    after (Left (Right n)) = Right (Right n)
    after (Right n) = Right (Left n)

sumGoI :: (Monad m) => VarName -> VarName -> TdGoI m ()
sumGoI varX varY = traceTd $ mapTd before after $ unliftUnit $ mprodTd hTd sumTd
  where
    before ::
      Either Flow (Either Nat Nat) -> Either (Either Nat Nat) (Trit, Nat)
    before (Left (Nothing, n)) = Left (Left n)
    before (Left (Just var, n))
      | var == varX  = Right (Second, n)
      | var == varY  = Right (Third, n)
      | otherwise  = error "Oops >_<"
    before (Right (Left n)) = Left (Right n)
    before (Right (Right n)) = Right (First, n)

    after ::
      Either (Either Nat Nat) (Trit, Nat) -> Either Flow (Either Nat Nat)
    after (Left (Left n)) = Left (Nothing, n)
    after (Left (Right n)) = Right (Right n)
    after (Right (First, n)) = Right (Left n)
    after (Right (Second, n)) = Left (Just varX, n)
    after (Right (Third, n)) = Left (Just varY, n)


variableGoI :: (Monad m) => VarName -> TdGoI m ()
variableGoI varX = mapTd before after hTd
  where
    before :: Flow -> Either Nat Nat
    before (Nothing, n) = Left n
    before (Just var, n)
      | var == varX  = Right n
      | otherwise  = error "Oops >_<"

    after :: Either Nat Nat -> Flow
    after (Left n) = (Nothing, n)
    after (Right n) = (Just varX, n)

applyGoI :: (Monad m) => TdGoI m x -> TdGoI m y -> TdGoI m (x, y)
applyGoI t s = traceTd $ mapTd before after $ mprodTd t s
  where
    before :: Either Flow (Either Nat Nat) -> Either Flow Flow
    before (Left (Nothing, n)) = Left (Nothing, phi . Left $ n)
    before (Left (Just var, n)) = case cc' n of
      Left k -> Left (Just var, k)
      Right k -> Right (Just var, k)
    before (Right (Left n)) = Left (Nothing, phi . Right $ n)
    before (Right (Right n)) = Right (Nothing, phi . (id +++ ee') . psi $ n)

    after :: Either Flow Flow -> Either Flow (Either Nat Nat)
    after (Left (Nothing, n)) = case psi n of
      Left k -> Left (Nothing, k)
      Right k -> Right (Right k)
    after (Left (Just var, n)) = Left (Just var, cc . Left $ n)
    after (Right (Nothing, n)) = Right (Left (phi . (id +++ ee) . psi $ n))
    after (Right (Just var, n)) = Left (Just var, cc . Right $ n)

abstGoI :: (Monad m, Pointed x, Eq x) =>
  VarName -> TdGoI m x -> TdGoI m (Func Nat x)
abstGoI varX t =
  traceTd $ mapTd before1 after1 $
    unliftUnit $ mprodTd hTd $ functorBoxTd $ mapTd before after t
  where
    before :: Flow -> Flow
    before (Nothing, n) = case psi n of
      Left k -> (Nothing, k)
      Right k -> (Just varX, k)
    before (Just var, n)
      | var == varX  = error "Oops >_<"
      | otherwise  = (Just var, n)

    after :: Flow -> Flow
    after (Nothing, n) = (Nothing, phi . Left $ n)
    after (Just var, n)
      | var == varX  = (Nothing, phi . Right $ n)
      | otherwise  = (Just var, n)

    before1 :: Either Flow (Either Nat Nat) -> Either (Either Nat Nat) (Nat, Flow)
    before1 (Left (Nothing, n)) = Left (Left n)
    before1 (Left (Just var, n)) = let
      (l, r) = vv . dd' $ n
      in Right (l, (Just var, r))
    before1 (Right (Left n)) = Left (Right n)
    before1 (Right (Right n)) = let
      (l, r) = vv n
      in Right (l, (Nothing, r))

    after1 :: Either (Either Nat Nat) (Nat, Flow) -> Either Flow (Either Nat Nat)
    after1 (Left (Left n)) = Left (Nothing, n)
    after1 (Left (Right n)) = Right (Right n)
    after1 (Right (l, (Nothing, r))) = Right (Left (uu (l, r)))
    after1 (Right (l, (Just var, r))) = Left (Just var, dd . uu $ (l, r))


{- primitive components of transducers: h, k, sum, cpy -}

hTd :: (Monad m) => Td m () (Either Nat Nat) (Either Nat Nat)
hTd = j0Td hh

kTd :: (Monad m) => Nat -> Td m () Nat Nat
kTd i = j0Td $ kk i

sumTd :: (Monad m) => Td m () (Trit, Nat) (Trit, Nat)
sumTd = j0Td summ

-- We do not need cpy ...
cpyTd :: (Monad m) => Td m () (Trit, Nat) (Trit, Nat)
cpyTd = j0Td func
  where
    func (First, i) = let
      (n, m) = vv i
      in (Third, uu (phi . Left $ n, m))
    func (Second, i) = let
      (n, m) = vv i
      in (Third, uu (phi . Right $ n, m))
    func (Third, i) = let
      (j, m) = vv i
      in case psi j of
        Left n -> (First, uu (n, m))
        Right n -> (Second, uu (n, m))

