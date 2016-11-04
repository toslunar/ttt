module Example where

import Lambda


stateExample = Asg "l" 2 $ Apply
  (Abst "x" $ sumLambda (Variable "x") (Asg "l" 3 $ Variable "x"))
  (Drf "l")

{- LOLA 2014 example 2 -}
idOne = Apply
  (Abst "x" $ Variable "x")
  (Const 1)

{- LOLA 2014 example 1 -}
threeOrFive = Oplus (Const 3) (Const 5)

-- sum t s := (\ x y -> x + y) t s
sumLambda lt ls = Apply (Apply
  (Abst "x" $ Abst "y" $ Sum "x" "y") lt) ls

-- (3 + 4) + 5
bindExample = sumLambda (sumLambda (Const 3) (Const 4)) (Const 5)

-- (\ x -> sum x x) (3 or 5)
firstNondetExample = Apply
  (Abst "x" $ sumLambda (Variable "x") (Variable "x"))
  (Oplus (Const 3) (Const 5))

-- (\ f -> sum (f 0) (f 1)) (\ x -> 3 or 5)
-- 2014.5.10
secondNondetExample = Apply
  (Abst "f" $ sumLambda
    (Apply (Variable "f") (Const 0))
    (Apply (Variable "f") (Const 1))
  )
  (Abst "x" $ Oplus (Const 3) (Const 5))

-- (\ x -> (\ y z -> y + z) x x) 3
secondPureExample = Apply
  (Abst "x" $ sumLambda (Variable "x") (Variable "x"))
  (Const 3)

-- (\ x y -> x + y) 5 3
firstPureExample = sumLambda (Const 5) (Const 3)
