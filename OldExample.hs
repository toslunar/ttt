{-# LANGUAGE NoMonomorphismRestriction #-}
module OldExample where

import Interpret
import InterpretLog

-- (\ x -> (\ y z -> y + z) x x) (3 or 5)
firstNondetExample =
  watchBounds "(\\ x -> (\\ y z -> y + z) x x) (3 or 5)" $ applyGoI
    (watchBounds "\\ x -> (\\ y z -> y + z) x x" $ abstGoI "x" $
      watchBounds "(\\ y z -> y + z) x x" $ applyGoI
        (watchBounds "(\\ y z -> y + z) x" $ applyGoI
          (watchBounds "\\ y z -> y + z" $ abstGoI "y" $
            watchBounds "\\ z -> y + z" $ abstGoI "z" $
              watchBounds "y + z" $ sumGoI "y" "z"
          )
          (watchBounds "x (left)" $ variableGoI "x")
        )
        (watchBounds "x (right)" $ variableGoI "x")
    )
    (watchBounds "3 or 5" $ oplusGoI
      (watchBounds "3" $ constGoI 3)
      (watchBounds "5" $ constGoI 5)
    )

-- \ t s -> t + s
additionGoI =
  watchBounds "\\ t s -> t + s" $ abstGoI "t" $
    watchBounds "\\ s -> t + s" $ abstGoI "s" $
      watchBounds "t + s" $ sumGoI "t" "s"

-- (\ f -> sum (f 0) (f 1)) (\ x -> 3 or 5)
-- 2014.5.10
secondNondetExample =
  watchBounds "(\\ f -> sum (f 0) (f 1)) (\\ x -> 3 or 5)" $ applyGoI
    (watchBounds "\\ f -> sum (f 0) (f 1))" $ abstGoI "f" $
      watchBounds "sum (f 0) (f 1)" $ applyGoI
        (watchBounds "sum (f 0)" $ applyGoI
          additionGoI
          (watchBounds "f 0" $ applyGoI
            (watchBounds "f (left)" $ variableGoI "f")
            (watchBounds "0" $ constGoI 0)
          )
        )
        (watchBounds "f 1" $ applyGoI
          (watchBounds "f (right)" $ variableGoI "f")
          (watchBounds "1" $ constGoI 1)
        )
    )
    (watchBounds "\\ x -> 3 or 5" $ abstGoI "x" $
      (watchBounds "3 or 5" $ oplusGoI
        (watchBounds "3" $ constGoI 3)
        (watchBounds "5" $ constGoI 5)
      )
    )

-- "x + x" is wrong!!
--   (watchBounds "(\\ x -> x + x) (3 or 5)" $ applyGoI
--     (watchBounds "\\ x -> x + x" $ abstGoI "x" $
--       watchBounds "x + x" $ sumGoI "x" "x"
--     )
--     (watchBounds "3 or 5" $ oplusGoI
--       (watchBounds "3" $ constGoI 3)
--       (watchBounds "5" $ constGoI 5)
--     )
--   )

-- (\ x y -> x + y) 3 (5 or 6)
theNonDetTd =
  (watchBounds "(\\ x y -> x + y) 3 (5 or 6)" $ applyGoI
    (watchBounds "(\\ x y -> x + y) 3" $ applyGoI
      (watchBounds "\\ x y -> x + y" $ abstGoI "x" $
        (watchBounds "\\ y -> x + y" $ abstGoI "y" $
          watchBounds "x + y" $ sumGoI "x" "y"
        )
      )
      (constGoI 3)
    )
    (watchBounds "5 or 6" $ oplusGoI
      (constGoI 5)
      (constGoI 6)
    )
  )

-- (\ x -> (\ y z -> y + z) x x) 3
secondPureExample =
  (watchBounds "(\\ x -> (\\ y z -> y + z) x x) 3" $ applyGoI
    (watchBounds "\\ x -> (\\ y z -> y + z) x x" $ abstGoI "x" $
      watchBounds "(\\ y z -> y + z) x x" $ applyGoI
        (watchBounds "(\\ y z -> y + z) x" $ applyGoI
          (watchBounds "\\ y z -> y + z" $ abstGoI "y" $
            watchBounds "\\ z -> y + z" $ abstGoI "z" $
              watchBounds "y + z" $ sumGoI "y" "z"
          )
          (watchBounds "x (left)" $ variableGoI "x")
        )
        (watchBounds "x (right)" $ variableGoI "x")
    )
    (watchBounds "3" $ constGoI 3)
  )

-- (\ x y -> x + y) 5 3
firstPureExample =
  (watchBounds "(\\ x y -> x + y) 5 3" $ applyGoI
    (watchBounds "(\\ x y -> x + y) 5" $ applyGoI
      (watchBounds "\\ x y -> x + y" $ abstGoI "x" $
        (watchBounds "\\ y -> x + y" $ abstGoI "y" $
          watchBounds "x + y" $ sumGoI "x" "y"
        )
      )
      (watchBounds "5" $ constGoI 5)
    )
    (watchBounds "3" $ constGoI 3)
  )

-- (\ x -> y) 3
wrongExample =
  watchBounds "(\\ x -> y) 3" $ applyGoI
    (watchBounds "\\ x -> y" $ abstGoI "x" $
      watchBounds "y" $ variableGoI "y"
    )
    (watchBounds "3" $ constGoI 3)
  
    
{-
theTd =
  applyGoI
    (applyGoI
      (abstGoI "x" $
        (abstGoI "y" $
          sumGoI "x" "y"
        )
      )
      (constGoI 3)
    )
    (constGoI 5)
-}
