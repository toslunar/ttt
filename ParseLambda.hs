module ParseLambda where

import Lambda
import Nat

import Control.Applicative ((<$>))

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
-- import Text.Parsec.Token
-- import Text.ParserCombinators.Parsec

parseExpr str =
  case parse pExpr "" str of
    Left err -> error $ show err
    Right e -> e

pExpr :: Parser LambdaExpr
pExpr = do
  e <- pExpr1
  eof
  return e

pExpr1, pExpr3, pExpr6, pExpr9, pExpr10, pAtom :: Parser LambdaExpr

pExpr1 =
  do
    asg <- try $ pAsg
    asg <$> pExpr1
  <|>
  do
    string "\\"
    xs <- pName `sepBy` spaces
    string "."
    -- Abst x <$> pExpr1
    flip (foldr Abst) xs <$> pExpr1
  <|>
  pExpr3

pAsg :: Parser (LambdaExpr -> LambdaExpr)
pAsg = do
  string "("
  loc <- pName
  string ":="
  n <- pNat
  string ");"
  return $ Asg loc n

pExpr3 = chainr1 pExpr6 (string "|_|" >> return Oplus)

pExpr6 = try pSum <|> pExpr9

pSum = do
  x <- pName
  string "+"
  y <- pName
  return $ Sum x y

pExpr9 =
  do
    string "!"
    loc <- pName
    return $ Drf loc
  <|>
  pExpr10

pExpr10 = chainl1 pAtom (spaces >> return Apply)

pAtom = between (string "(") (string ")") pExpr1 <|> (Variable <$> pName) <|> (ConstN <$> pNat)


pNat :: Parser Nat
pNat = do
  ds <- many1 $ oneOf ['0'..'9']
  return $ fromInteger $ read ds

pName :: Parser String
pName = many1 $ oneOf (['a'..'z']++['A'..'Z'])

-- spaces = many $ oneOf [' ','\t','\n']

