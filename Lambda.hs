{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# NoMonomorphismRestriction #-}
module Lambda where

import InterpretAlgOpr
import InterpretPure
import Interpret
import Nondeterministic
import Memory
import Nat
import Log
import Pointed
import MyShow

import Control.Monad.Writer.Class
import Control.Monad.State.Class
-- import qualified Data.Map as M


type ExistTdGoI m =
  forall r . (forall x . (Pointed x, Eq x, MyShow x) => TdGoI m x -> r) -> r

type LocationExpr = String

data LambdaExpr =
  -- symbol
    Const Nat
  | Sum VarName VarName
  -- lambda
  | Variable VarName
  | Apply LambdaExpr LambdaExpr
  | Abst VarName LambdaExpr
  -- algebraic operation
  | Oplus LambdaExpr LambdaExpr
  | Drf LocationExpr
  | Asg LocationExpr Nat LambdaExpr
  -- hole (like evaluation context)
  | Hole LambdaExpr
  deriving (Show)

instance MyShow LambdaExpr where
  myShowsPrec d (Const n) = shows n
  myShowsPrec d (Sum varX varY) = showParen (d > 6) $
    showString varX .
    showString "+" .
    showString varY
  myShowsPrec d (Variable varX) = showString varX
  myShowsPrec d (Apply l1 l2) = showParen (d > 10) $
    myShowsPrec 10 l1 .
    showString " " .
    myShowsPrec 11 l2
  myShowsPrec d (Abst varX l) = showParen (d > 0) $
    showString ("\\" ++ varX ++ ".") .
    myShowsPrec 0 l
  myShowsPrec d (Oplus l1 l2) = showParen (d > 2) $
    myShowsPrec 3 l1 .
    showString "|_|" .
    myShowsPrec 3 l2
  myShowsPrec d (Drf loc) = showParen (d > 9) $
    showString "!" .
    myShowsPrec 9 (Location loc)
  myShowsPrec d (Asg loc n l) = showParen (d > 1) $
    showString "(" .
    myShowsPrec 2 (Location loc) .
    showString ":=" .
    showsPrec 2 n .
    showString ");" .
    myShowsPrec 1 l
  myShowsPrec d (Hole l) =
    showString "[" .
    myShowsPrec 0 l .
    showString "]"


{-
exist0 :: (Pointed x, Eq x, MyShow x) => TdGoI m x -> ExistTdGoI m
exist0 f = \ k -> k f

-- exist1 :: (Pointed x, Eq x) =>
--   (TdGoI m x -> TdGoI m x) -> ExistTdGoI m -> ExistTdGoI m
-- exist1 f x = \ k :: ((forall x . (Pointed x, Eq x) => TdGoI m x -> r) -> r) ->
--   x $ \ a -> k (f a)
-}

{-
interpretPure :: (Monad m) => LambdaExpr -> ExistTdGoI m
interpretPure (Const n) = \ k -> k $ constGoI n
interpretPure (Sum varX varY) = \ k -> k $ sumGoI varX varY
interpretPure (Variable varX) = \ k -> k $ variableGoI varX
interpretPure (Apply l1 l2) = \ k ->
  interpretPure l1 $ \ a ->
  interpretPure l2 $ \ b -> k $ applyGoI a b
interpretPure (Abst varX l) = \ k ->
  interpretPure l $ \ a -> k $ abstGoI varX a

interpretNondet :: (MonadNondet m) => LambdaExpr -> ExistTdGoI m
interpretNondet (Const n) = \ k -> k $ constGoI n
interpretNondet (Sum varX varY) = \ k -> k $ sumGoI varX varY
interpretNondet (Variable varX) = \ k -> k $ variableGoI varX
interpretNondet (Apply l1 l2) = \ k ->
  interpretNondet l1 $ \ a ->
  interpretNondet l2 $ \ b -> k $ applyGoI a b
interpretNondet (Abst varX l) = \ k ->
  interpretNondet l $ \ a -> k $ abstGoI varX a
interpretNondet (Oplus l1 l2) = \ k ->
  interpretNondet l1 $ \ a ->
  interpretNondet l2 $ \ b -> k $ oplusGoI a b
-}

interpretPure ::
  (MonadWriter Log m) => LambdaExpr -> ExistTdGoI m
interpretPure = go myShow  where
  go :: (MonadWriter Log m) =>
    (LambdaExpr -> String) -> LambdaExpr -> ExistTdGoI m
  go sh (Const n) = \ k ->
    k $ constGoI (sh . Hole $ Const n) n
  go sh (Sum varX varY) = \ k ->
    k $ sumGoI (sh . Hole $ Sum varX varY) varX varY
  go sh (Variable varX) = \ k ->
    k $ variableGoI (sh . Hole $ Variable varX) varX
  go sh (Apply l1 l2) = \ k ->
    go (\ m -> sh $ Apply m l2) l1 $ \ a ->
    go (\ m -> sh $ Apply l1 m) l2 $ \ b ->
      k $ applyGoI (sh . Hole $ Apply l1 l2) a b
  go sh (Abst varX l) = \ k ->
    go (\ m -> sh $ Abst varX m) l $ \ a ->
      k $ abstGoI (sh . Hole $ Abst varX l) varX a

interpretNondet ::
  (MonadNondet m, MonadWriter Log m) => LambdaExpr -> ExistTdGoI m
interpretNondet = go myShow  where
  go :: (MonadNondet m, MonadWriter Log m) =>
    (LambdaExpr -> String) -> LambdaExpr -> ExistTdGoI m
  go sh (Const n) = \ k ->
    k $ constGoI (sh . Hole $ Const n) n
  go sh (Sum varX varY) = \ k ->
    k $ sumGoI (sh . Hole $ Sum varX varY) varX varY
  go sh (Variable varX) = \ k ->
    k $ variableGoI (sh . Hole $ Variable varX) varX
  go sh (Apply l1 l2) = \ k ->
    go (\ m -> sh $ Apply m l2) l1 $ \ a ->
    go (\ m -> sh $ Apply l1 m) l2 $ \ b ->
      k $ applyGoI (sh . Hole $ Apply l1 l2) a b
  go sh (Abst varX l) = \ k ->
    go (\ m -> sh $ Abst varX m) l $ \ a ->
      k $ abstGoI (sh . Hole $ Abst varX l) varX a
  go sh (Oplus l1 l2) = \ k ->
    go (\ m -> sh $ Oplus m l2) l1 $ \ a ->
    go (\ m -> sh $ Oplus l1 m) l2 $ \ b ->
      k $ oplusGoI (sh . Hole $ Oplus l1 l2) a b

interpretState :: (MonadState Memory m, MonadWriter Log m) =>
  LambdaExpr -> ExistTdGoI m
interpretState = go myShow  where
  go :: (MonadState Memory m, MonadWriter Log m) =>
    (LambdaExpr -> String) -> LambdaExpr -> ExistTdGoI m
  go sh (Const n) = \ k ->
    k $ constGoI (sh . Hole $ Const n) n
  go sh (Sum varX varY) = \ k ->
    k $ sumGoI (sh . Hole $ Sum varX varY) varX varY
  go sh (Variable varX) = \ k ->
    k $ variableGoI (sh . Hole $ Variable varX) varX
  go sh (Apply l1 l2) = \ k ->
    go (\ m -> sh $ Apply m l2) l1 $ \ a ->
    go (\ m -> sh $ Apply l1 m) l2 $ \ b ->
      k $ applyGoI (sh . Hole $ Apply l1 l2) a b
  go sh (Abst varX l) = \ k ->
    go (\ m -> sh $ Abst varX m) l $ \ a ->
      k $ abstGoI (sh . Hole $ Abst varX l) varX a
  go sh (Drf loc) = \ k ->
    k $ drfGoI (sh . Hole $ Drf loc) (Location loc)
  go sh (Asg loc n l) = \ k ->
    go (\ m -> sh $ Asg loc n m) l $ \ a ->
      k $ asgGoI (sh . Hole $ Asg loc n l) (Location loc) n a

