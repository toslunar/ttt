{-# LANGUAGE NoMonomorphismRestriction #-}
import Test
import Example
import ParseLambda
import Lambda

import Data.List
import Control.Monad
import Control.Applicative
import System.IO (isEOF)

main = do
  b <- isEOF
  unless b $ do
    putStrLn $ replicate 100 '#'
    t <- parseExpr <$> getLine
    putStrLn $ "parsed: " ++ show t
    putStrLn $ replicate 100 '='
    case  effects t  of
      []          -> interpretPure   t $ testPure
      [EffNondet] -> interpretNondet t $ testNondet
      [EffState]  -> interpretState  t $ testState
    main


data EffectType = EffNondet | EffState
  deriving Eq

effects :: LambdaExpr -> [EffectType]
effects (Oplus l1 l2) = nub $ [EffNondet] ++ effects l1 ++ effects l2
effects (Drf _) = [EffState]
effects (Asg _ _ l) = nub $ [EffState] ++ effects l
effects (Apply l1 l2) = nub $ effects l1 ++ effects l2
effects (Abst _ l) = effects l
effects (Hole _) = error "Do not input ...[...]..."
effects _ = []

mainOld = do
  shout "(\\ x y -> x + y) 5 3"
  interpretPure firstPureExample $ testPure 

  shout "(\\ x -> x + x) 3"
  interpretPure secondPureExample $ testPure

  shout "(\\ x -> x + x) (3 |_| 5)"
  interpretNondet firstNondetExample $ testNondet

  shout "(\\ f -> f 0 + f 1) (\\ x -> 3 |_| 5)"
  interpretNondet secondNondetExample $ testNondet

  shout "(l := 2); (\\ x -> x + (l := 3); x) (! l)"
  interpretState stateExample $ testState

shout str = mapM_ putStrLn $
  [replicate 100 '#', str, replicate 100 '=']
