module Memory where

import Nat
import MyShow

import qualified Data.Map as M


newtype Location = Location String
  deriving (Eq, Ord, Show)

instance MyShow Location where
  myShowsPrec d (Location str) = showString str


type Memory = M.Map Location Nat

emptyMemory :: Memory
emptyMemory = M.empty
