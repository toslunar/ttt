{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module InterpretPure where

import Interpret
import Transducer
import Nat
import Log
import FuncType
import Pointed
import MyShow

import Control.Monad
import Control.Monad.Writer.Class



constGoI str i    = watchBounds str $ constGoILog i
sumGoI str x y    = watchBounds str $ sumGoILog x y
variableGoI str x = watchBounds str $ variableGoILog x
applyGoI str t s  = watchBounds str $ applyGoILog t s
abstGoI str t x   = watchBounds str $ abstGoILog t x


functorBoxTdLog :: (MonadWriter Log m, Pointed x, Eq x) =>
  Td m x a b -> Td m (Func Nat x) (Nat, a) (Nat, b)
functorBoxTdLog t = Td $ \ a -> do
  tell $ Log [LogStrLn $ show (fst a) ++ " {", Indent]
  b <- getTd (functorBoxTd t) a
  tell $ Log [Unindent, LogStrLn $ "} " ++ show (fst b)]
  return b


constGoILog :: (MonadWriter Log m) => Nat -> TdGoI m ()
constGoILog i =
  traceTd $ mapTd before after $ mprodTd' hLog (jTd $ kLog i)
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

sumGoILog :: (MonadWriter Log m) => VarName -> VarName -> TdGoI m ()
sumGoILog varX varY = traceTd $ mapTd before after $ mprodTd' hLog (jTd $ sumLog)
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

variableGoILog :: (MonadWriter Log m) => VarName -> TdGoI m ()
variableGoILog varX = mapTd before after $ jTd hLog
  where
    before :: Flow -> Either Nat Nat
    before (Nothing, n) = Left n
    before (Just var, n)
      | var == varX  = Right n
      | otherwise  = error "Oops >_<"

    after :: Either Nat Nat -> Flow
    after (Left n) = (Nothing, n)
    after (Right n) = (Just varX, n)

applyGoILog :: (MonadWriter Log m) => TdGoI m x -> TdGoI m y -> TdGoI m (x, y)
applyGoILog t s = traceTd $ mapTd' before after $ mprodTd t s
  where
    -- before :: Either Flow (Either Nat Nat) -> m (Either Flow Flow)
    before (Left (Nothing, n)) = return . Left . (Nothing,) <=< phiLog . Left $ n
    before (Left (Just var, n)) = c'Log n >>= \ case
      Left k  -> return $ Left  (Just var, k)
      Right k -> return $ Right (Just var, k)
    before (Right (Left n)) = return . Left . (Nothing,) <=< phiLog . Right $ n
    before (Right (Right n)) = return . Right . (Nothing,) <=< phiLog <=<
      either (liftM Left . return) (liftM Right . e'Log) <=< psiLog $ n

    -- after :: Either Flow Flow -> m (Either Flow (Either Nat Nat))
    after (Left (Nothing, n)) = psiLog n >>= \ case
      Left k  -> return $ Left  (Nothing, k)
      Right k -> return $ Right (Right k)
    after (Left (Just var, n)) = return . Left . (Just var,) <=< cLog . Left $ n
    after (Right (Nothing, n)) = return . Right . Left <=< phiLog <=<
      either (liftM Left . return) (liftM Right. eLog) <=< psiLog $ n
    after (Right (Just var, n)) = return . Left . (Just var,) <=< cLog . Right $ n

abstGoILog :: (MonadWriter Log m, Pointed x, Eq x) =>
  VarName -> TdGoI m x -> TdGoI m (Func Nat x)
abstGoILog varX t =
  traceTd $ mapTd' before1 after1 $
    mprodTd' hLog $ functorBoxTdLog $ mapTd' before after t
  where
    -- before :: Flow -> m Flow
    before (Nothing, n) = psiLog n >>= \ case
      Left k -> return (Nothing, k)
      Right k -> return (Just varX, k)
    before (Just var, n)
      | var == varX  = return $ error "Oops >_<"
      | otherwise  = return (Just var, n)

    -- after :: Flow -> m Flow
    after (Nothing, n) = return . (Nothing,) <=< phiLog . Left $ n
    after (Just var, n)
      | var == varX  = return . (Nothing,) <=< phiLog . Right $ n
      | otherwise  = return (Just var, n)

    -- before1 :: Either Flow (Either Nat Nat) -> m (Either (Either Nat Nat) (Nat, Flow))
    before1 (Left (Nothing, n)) = return $ Left (Left n)
    before1 (Left (Just var, n)) = do
      (l, r) <- vLog <=< d'Log $ n
      return $ Right (l, (Just var, r))
    before1 (Right (Left n)) = return $ Left (Right n)
    before1 (Right (Right n)) = do
      (l, r) <- vLog n
      return $ Right (l, (Nothing, r))

    -- after1 :: Either (Either Nat Nat) (Nat, Flow) -> m (Either Flow (Either Nat Nat))
    after1 (Left (Left n)) = return $ Left (Nothing, n)
    after1 (Left (Right n)) = return $ Right (Right n)
    after1 (Right (l, (Nothing, r))) = return . Right . Left <=< uLog $ (l, r)
    after1 (Right (l, (Just var, r))) = return . Left . (Just var,) <=< dLog <=< uLog $ (l, r)


{- pure functions with logging -}

addLog :: (MonadWriter Log m) => String -> (a -> b) -> a -> m b
addLog str func = \ a -> do
  tell $ Log [LogStr str]
  return $ func a

hLog :: (MonadWriter Log m) => Either Nat Nat -> m (Either Nat Nat)
hLog = addLog "h" hh

kLog :: (MonadWriter Log m) => Nat -> Nat -> m Nat
kLog i = addLog ("k_" ++ show i) (kk i)

sumLog :: (MonadWriter Log m) => (Trit, Nat) -> m (Trit, Nat)
sumLog = addLog "sum" summ

cLog = addLog "c" cc

c'Log = addLog "c'" cc'

dLog = addLog "d" dd

d'Log = addLog "d'" dd'

eLog = addLog "e" ee

e'Log = addLog "e'" ee'

phiLog = addLog "phi" phi

psiLog = addLog "psi" psi

uLog = addLog "u" uu

vLog = addLog "v" vv
