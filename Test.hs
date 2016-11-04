{-# LANGUAGE NoMonomorphismRestriction #-}
module Test where

import Interpret
import AlgOpr
import Nondeterministic
import Memory
import Nat
import Log
import Pointed
import Arrow
import MyShow

import Control.Monad
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.IO.Class
import Test.QuickCheck
-- import Data.Functor.Identity

gauche, droit :: Nat -> Nat
gauche = phi . Left
droit = phi . Right

pair :: Nat -> Nat -> Nat
pair n m = uu (n, m)

run :: (Monad m, Pointed x) => TdGoI m x -> Nat -> Nat -> m (Nat, x)
run t n m = do
  (res, x) <- runGoI t (droit . droit $ pair n m)
  case psi res of
    Left i -> return . error $ "run: the result is (g " ++ show i ++ ")."
    Right i ->
      case psi i of
        Left j -> return . error $ "run: the result is (d (g " ++ show j ++ "))."
        Right j -> let
          (k, l) = vv j
          in if k /= n
            then return . error $ "run: the result is (d (d <" ++ show k ++ ", " ++ show l ++ ">)), " ++ show k ++ " /= " ++ show n ++ "."
            else return (l, x)

run' t = run t 42 137

showResult :: (MyShow x) => (Nat, x) -> String
showResult (n, x) = "Result: " ++ show n ++ " / State: " ++ myShow x

testPure :: (Pointed x, MyShow x) => TdGoI (Writer Log) x -> IO ()
testPure td = let
  (res, w) = runWriter $ run' td
  in do
    mapM_ putStrLn $ showLog w
    putStrLn $ showResult res

-- testNondet :: (Pointed x, MonadNondet m, MyShow x) =>
testNondet :: (Pointed x, MyShow x) => TdGoI (NondetWriter Log) x -> IO ()
testNondet td = putStr $
  drawTreeBy showLog' 0 showResult (run' td)

testState :: (Pointed x, MyShow x) => TdGoI (RWS () Log Memory) x -> IO ()
testState td = let
  (res, s, w) = runRWS (run' td) () emptyMemory
  in do
    mapM_ putStrLn $ showLog w
    print s
    putStrLn $ showResult res


{- identities of functions u, v, phi, psi -}
testFunctions = do
  testUV
  testVU
  testPhiPsi
  testPsiPhi

testUV = quickCheck $ \ n' -> n' >= 0 ==>
  let n = fromInteger n' in n == uu (vv n)

testVU = quickCheck $ \ n' m' -> n' >= 0 && m' >= 0 ==> let
  n = fromInteger n'
  m = fromInteger m'
  in (n, m) == vv (uu (n, m))

testPhiPsi = quickCheck $ \ n' -> n' >= 0 ==>
  let n = fromInteger n' in n == phi (psi n)

testPsiPhi = quickCheck $ \ e' -> either (>=0) (>=0) e' ==> let
  e = (fromInteger +++ fromInteger) e'
  in e == psi (phi e)
