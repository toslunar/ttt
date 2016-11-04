{-# LANGUAGE NoMonomorphismRestriction #-}
import Test
import Example
import Lambda

main = do
  shout "(\\ x y -> x + y) 5 3"
  interpretPure firstPureExample $ testPure 

  shout "(\\ x -> x + x) 3"
  interpretPure secondPureExample $ testPure

  shout "(\\ x -> x + x) (3 or 5)"
  interpretNondet firstNondetExample $ testNondet

  shout "(\\ f -> f 0 + f 1) (\\ x -> 3 or 5)"
  interpretNondet secondNondetExample $ testNondet

  shout "(l := 2); (\\ x -> x + (l := 3); x) (! l)"
  interpretState stateExample $ testState

shout str = mapM_ putStrLn $
  [replicate 100 '#', str, replicate 100 '=']
