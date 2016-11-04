module Example where

import Lambda


stateExample = Asg "l" 2 $ Apply
  (Abst "x" $ sumLambda (Variable "x") (Asg "l" 3 $ Variable "x"))
  (Drf "l")

{- LOLA 2014 example 2 -}
idOne = Apply
  (Abst "x" $ Variable "x")
  (ConstN 1)

{- LOLA 2014 example 1 -}
threeOrFive = Oplus (ConstN 3) (ConstN 5)

-- sum t s := (\ x y -> x + y) t s
sumLambda lt ls = Apply (Apply
  (Abst "x" $ Abst "y" $ Sum "x" "y") lt) ls

-- (3 + 4) + 5
bindExample = sumLambda (sumLambda (ConstN 3) (ConstN 4)) (ConstN 5)

-- (\ x -> sum x x) (3 or 5)
firstNondetExample = Apply
  (Abst "x" $ sumLambda (Variable "x") (Variable "x"))
  (Oplus (ConstN 3) (ConstN 5))

-- (\ f -> sum (f 0) (f 1)) (\ x -> 3 or 5)
-- 2014.5.10
secondNondetExample = Apply
  (Abst "f" $ sumLambda
    (Apply (Variable "f") (ConstN 0))
    (Apply (Variable "f") (ConstN 1))
  )
  (Abst "x" $ Oplus (ConstN 3) (ConstN 5))

-- (\ x -> (\ y z -> y + z) x x) 3
secondPureExample = Apply
  (Abst "x" $ sumLambda (Variable "x") (Variable "x"))
  (ConstN 3)

-- (\ x y -> x + y) 5 3
firstPureExample = sumLambda (ConstN 5) (ConstN 3)
