{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import           Control.Applicative
import           Control.Monad       (forM_)
import           Control.Monad       (forever)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Text.Parsec         hiding (many, (<|>))
import           Text.Parsec.Text
import           Text.Parser.Token   (integerOrDouble, symbol)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Num Double
          deriving Show


num :: Parser Expr
num = Num . (either fromIntegral id) <$> integerOrDouble

addop :: Parser (Expr -> Expr -> Expr)
addop  = Add <$ symbol "+"
      <|> Sub <$ symbol "-"

mulop :: Parser (Expr -> Expr -> Expr)
mulop  = Mul <$ symbol "*"
      <|> Div <$ symbol "/"

term :: Parser Expr
term = choice [ num ] <* spaces

expr :: Parser Expr
expr = term `chainl1` (addop <|> mulop)

eval :: Expr -> Double
eval (Num n) = n
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y

evalText :: Text -> IO()
evalText str =
    case parse expr "" str of
      Left e -> error (show e)
      Right x -> print (eval x)

parseFile :: FilePath -> IO ()
parseFile path = do
  txt <-  T.readFile path
  forM_ (T.lines txt) evalText

repl = forever $ evalText . T.pack =<< getLine
