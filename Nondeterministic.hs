{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- for "instance MonadWriter w (NondetWriter w)"
module Nondeterministic
  (
    MonadNondet (..)
  , NondetWriter
  , drawTreeBy
  ) where

import Data.Monoid
-- import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer.Class


class (Monad m) => MonadNondet m where
  chooseNondet :: [m a] -> m a


data NondetWriter w a =
    Leaf w a
  | Node w [NondetWriter w a]
  deriving Show


instance (Monoid w) => Monad (NondetWriter w) where
  return a = Leaf mempty a

  (Leaf w a) >>= f = case f a of
    Leaf v b -> Leaf (w <> v) b
    Node v b -> Node (w <> v) b
  (Node w trees) >>= f = Node w $ map (>>= f) trees

instance (Monoid w) => MonadNondet (NondetWriter w) where
  chooseNondet = Node mempty

instance (Monoid w) => MonadWriter w (NondetWriter w) where
  writer (a, w) = Leaf w a

  listen = go mempty  where
    go v (Leaf w a) = Leaf w (a, v <> w)
    go v (Node w trees) = Node w $ map (go (v <> w)) trees

  -- Warning: this implementation of pass might be correct only for some good f
  pass (Leaf w (a, f)) = Leaf (f w) a
  pass (Node w trees) = Node w $ map pass trees


depth :: NondetWriter w a -> Int
depth (Leaf _ _) = 0
depth (Node _ trees) = 1 + maximum (0 : map depth trees)

drawTreeBy ::
  (s -> w -> (s, [String])) -> s -> (a -> String) -> NondetWriter w a -> String
drawTreeBy showw showwAux showa t =
  unlines . map (softTab (2 * depth t + 3)) . flip runReader showwAux . draw $ t
  where
    softTab :: Int -> (String, String) -> String
    softTab _ (strl, "") = strl
    softTab tabstop (strl, strr) = (take tabstop $ strl ++ repeat ' ') ++ strr

    showw' w cont = do
      aux <- ask
      let (aux', strs) = showw aux w
      rest <- local (const aux') cont
      return $ zip ("." : repeat "|") strs ++ rest

    -- draw :: NondetWriter w a -> [(String, String)]
    draw (Leaf w a) = showw' w $ return [(showa a, "")]
    draw (Node w trees) = showw' w $ drawSubTrees trees

    drawSubTrees [] = return [("$", "")]
    drawSubTrees [t] = shift "`-" "  " <$> draw t
    drawSubTrees (t : ts) = (++) <$>
      (shift "+-" "| " <$> draw t) <*>
      (drawSubTrees ts)

    shift first other =
      zipWith (\ sh (l, r) -> (sh ++ l, r)) (first : repeat other)
-- (cf. Data.Tree)
{-
drawTreeBy :: (w -> [String]) -> (a -> String) -> NondetWriter w a -> String
drawTreeBy showw showa t = unlines . map (softTab (2 * depth t + 3)) . draw $ t
  where
    softTab :: Int -> (String, String) -> String
    softTab _ (strl, "") = strl
    softTab tabstop (strl, strr) = (take tabstop $ strl ++ repeat ' ') ++ strr

    showw' = zip ("." : repeat "|") . showw

    -- draw :: NondetWriter w a -> [(String, String)]
    draw (Leaf w a) = showw' w ++ [(showa a, "")]
    draw (Node w trees) = showw' w ++ drawSubTrees trees

    drawSubTrees [] = [("$", "")]
    drawSubTrees [t] = shift "`-" "  " (draw t)
    drawSubTrees (t : ts) = shift "+-" "| " (draw t) ++ drawSubTrees ts

    shift first other =
      zipWith (\ sh (l, r) -> (sh ++ l, r)) (first : repeat other)
-}
