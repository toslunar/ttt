{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
module TransducerAlgOpr where

import Transducer
import AlgOpr
import Nondeterministic
import Pointed
import Arrow

import Control.Monad.State


algOprTd :: (Monad m, Pointed x) =>
  AlgOpr m i -> (i -> Td m x a b) -> Td m (Maybe (i, x)) a b
algOprTd alpha ts = Td $ \ a -> StateT $ maybe
  (($ a) $ alpha $ \ ix ->
    liftM (id *** Just . (ix,)) . flip runStateT point . getTd (ts ix)
  )
  (\ (ix, x) ->
    liftM (id *** Just . (ix,)) . runStateT (getTd (ts ix) a) $ x
  )

binaryAlgOprTd :: (Monad m, Pointed x, Pointed y) =>
  AlgOpr m Bool -> Td m x a b -> Td m y a b -> Td m (Maybe (Either x y)) a b
binaryAlgOprTd alpha s t = Td $ \ a -> StateT $ maybe
  (($ a) $ alpha $ \ bit -> case bit of
    False -> liftM (id *** Just . Left ) . flip runStateT point . getTd s
    True  -> liftM (id *** Just . Right) . flip runStateT point . getTd t
  )
  (either
    (liftM (id *** Just . Left ) . runStateT (getTd s a))
    (liftM (id *** Just . Right) . runStateT (getTd t a))
  )


oplusTd :: (MonadNondet m, Pointed x, Pointed y) =>
  Td m x a b -> Td m y a b -> Td m (Maybe (Either x y)) a b
oplusTd = binaryAlgOprTd oplusOpr

lookupTd loc = algOprTd (lookupOpr loc)

updateTd loc val = algOprTd (updateOpr loc val)
