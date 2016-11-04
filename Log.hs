{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Log
  ( Log (Log)
  , Event (LogStrLn, LogStr, Indent, Unindent)
  , showLog, showLog'
  ) where

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.RWS


indentWidth = 2 :: Int

newtype Log = Log { getLog :: [Event] }
  deriving Monoid
data Event =
    LogStrLn String
  | LogStr String
  | Indent
  | Unindent


fromLogStr :: Event -> Maybe String
fromLogStr (LogStr s) = Just s
fromLogStr _ = Nothing

showLog :: Log -> [String]
showLog = snd . showLog' 0

-- it sets the number of indents at the beginning, and
-- returns with the number of indents at the end
showLog' :: Int -> Log -> (Int, [String])
showLog' i l = let
  actions = map indent . getLog . mergeLogStr $ l
  in execRWS (sequence_ actions) () i
  where
    indent (LogStrLn s) = do
      i <- get
      tell [replicate (indentWidth * i) ' ' ++ s]
    indent Indent = modify (+ 1)
    indent Unindent = modify (subtract 1)

-- showLog = map addIndent . getIndent . mergeLogStr  where
--   addIndent (i, s) = replicate (2*i) ' ' ++ s

mergeLogStr :: Log -> Log
mergeLogStr = Log . map g . groupBy f . getLog  where
  f (LogStr _) (LogStr _) = True
  f _ _ = False

  g es = if fromLogStr (head es) /= Nothing
    then LogStrLn $ intercalate "; " $ map (fromJust . fromLogStr) es
    else head es
   
{-
getIndent :: Log -> [(Int, String)]
getIndent = go 0 . getLog  where
  go _ [] = []
  go i (Indent : es) = go (i+1) es
  go i (Unindent : es) = go (i-1) es
  go i ((LogStrLn str) : es) = (i, str) : go i es
-}
