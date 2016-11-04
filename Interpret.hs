{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpret where

-- import TransducerAlgOpr
import Transducer
-- import Nondeterministic
import Nat
import Log
-- import FuncType
import Pointed
import Arrow
import MyShow

import Control.Monad.Writer.Class
import Control.Monad.State.Class

-- for wires of transducer
type VarName = String
type Flow = (Maybe VarName, Nat)
type TdGoI m x = Td m x Flow Flow


runGoI :: (Monad m, Pointed x) => TdGoI m x -> Nat -> m (Nat, x)
runGoI = runTd . hideVar

hideVar :: (Monad m) => TdGoI m x -> Td m x Nat Nat
hideVar = mapTd before after
  where
    before n = (Nothing, n)
    after (Nothing, n) = n
    after (Just var, n) = error "Oops >_<"

-- give name, and watch boundaries
watchBounds :: (MonadWriter Log m, MyShow x) =>
  String -> TdGoI m x -> TdGoI m x
watchBounds name t = Td $ \ a -> do
  x0 <- get
  tell $ Log  -- $ if bad a then [] else
    [ LogStrLn $ "---- " ++ show (snd a) ++ " --->"
    , LogStrLn $ showIn (fst a) ++ ":[ " ++ name ++ " @ " ++ myShow x0 ++ " ]"
    , Indent
    ]
  b <- getTd t a
  x1 <- get
  tell $ Log  -- $ if bad b then [] else
    [ Unindent
    , LogStrLn $ "[ " ++ name ++ " @ " ++ myShow x1 ++ " ]:" ++ showOut (fst b)
    , LogStrLn $ "---- " ++ show (snd b) ++ " --->"
    ]
  return b
  where
    showIn, showOut :: Maybe VarName -> String
    showIn Nothing = "Query"
    showIn (Just var) = "Answer " ++ var
    showOut Nothing = "Answer"
    showOut (Just var) = "Query " ++ var

    -- bad (Nothing, n) = case psi n of
    --   Left _ -> True
    --   Right k -> case psi k of
    --     Left _ -> True
    --     Right _ -> False
    -- bad (Just _, _) = False


{- functions h, k_i, sum -}

hh :: Either Nat Nat -> Either Nat Nat
hh = either
  (either
    (Left . phi . Right . phi . Left)
    (either
      (Left . phi . Left)
      (Right)
    . psi)
  . psi)
  (Left . phi . Right . phi . Right)

kk :: Nat -> Nat -> Nat
kk i = uu . (id *** const i) . vv

data Trit = First | Second | Third
  deriving Eq

summ :: (Trit, Nat) -> (Trit, Nat)
summ (First, n) = (Second, n)
summ (Second, n) = (Third, uu (n, 0))
summ (Third, i) = let
  (j, l) = vv i
  (n, m) = vv j
  in (First, uu (n, m + l))

{- functions using u, v, phi, psi -}

ee' :: Nat -> Nat
ee' = snd . vv

ee :: Nat -> Nat
ee = uu . (\ n -> (0, n))  -- why (1, n) in the paper?

cc' :: Nat -> Either Nat Nat
cc' = (uu +++ uu) . distribute . (psi *** id) . vv

cc :: Either Nat Nat -> Nat
cc = uu . (phi *** id) . distribute' . (vv +++ vv)

distribute :: (Either a b, c) -> Either (a, c) (b, c)
distribute (Left a, c) = Left (a, c)
distribute (Right b, c) = Right (b, c)

distribute' :: Either (a, c) (b, c) -> (Either a b, c)
distribute' (Left (a, c)) = (Left a, c)
distribute' (Right (b, c)) = (Right b, c)

dd' :: Nat -> Nat
dd' = uu . (id *** uu) . (\ ((a, b), c) -> (a, (b, c))) . (vv *** id) . vv

dd :: Nat -> Nat
dd = uu . (uu *** id) . (\ (a, (b, c)) -> ((a, b), c)) . (id *** vv) . vv
