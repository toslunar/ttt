module Nat
  (
    Nat
  , uu, vv, phi, psi
  ) where

{- switch of the implementation of Nat -}
import NatEncoded
-- import NatRaw

import MyShow

instance MyShow Nat where
  myShowsPrec = showsPrec
