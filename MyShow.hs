module MyShow where

import GHC.Show (appPrec, appPrec1)

class MyShow a where
  myShow :: a -> String
  myShowsPrec :: Int -> a -> ShowS
  -- default implementations
  myShow x = myShows x ""
  myShowsPrec _ x s = myShow x ++ s

myShows :: (MyShow a) => a -> ShowS
myShows = myShowsPrec 0

instance MyShow () where
  myShowsPrec d () = showParen (d > 0) $
    showString "*"

instance (MyShow a, MyShow b) => MyShow (a, b) where
  myShowsPrec d (x, y) =
    showString "(" .
    myShows x .
    showString ", " .
    myShows y .
    showString ")"

instance (MyShow a, MyShow b) => MyShow (Either a b) where
    myShowsPrec p (Left x) s
                          = (showParen (p > appPrec) $
                             showString "Left " .
                             myShowsPrec appPrec1 x) s
    myShowsPrec p (Right x) s
                          = (showParen (p > appPrec) $
                             showString "Right " .
                             myShowsPrec appPrec1 x) s

-- copied from Prelude
instance MyShow a => MyShow (Maybe a) where
    myShowsPrec _p Nothing s = showString "Nothing" s
    myShowsPrec p (Just x) s
                          = (showParen (p > appPrec) $
                             showString "Just " .
                             myShowsPrec appPrec1 x) s
