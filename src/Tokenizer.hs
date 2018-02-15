module Tokenizer (Operator(..), Token(..), tokenize, showToken) where

import Prelude
import Data.Char
import Data.List

data Operator = Plus
              | Minus
              | Power
              | Times
              | Div
              | Root
              | PLeft
              | PRight
              deriving (Show, Eq)

data Token = TOperator Operator
           | TSpace
           | TNumber Int
           deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '^' = Power
           | c == 'v' = Root
           | c == '(' = PLeft
           | c == ')' = PRight
           | otherwise = error $ "Bad input Operator " ++ [c]

tokenizeNumber :: String -> [Token]
tokenizeNumber str = let (digits, rest) = span isDigit str in
                        TNumber (read digits) : tokenize rest

tokenize :: String -> [Token]
tokenize [] = []
tokenize (h:t) | elem h "+/*-^v()" = TOperator (operator h) : tokenize t
               | isDigit h = tokenizeNumber (h:t)
               | isSpace h = tokenize t
               | otherwise = error $ "Bad tokenizer input \"" ++ [h] ++ "\""

-- Debug Purpose

opToStr :: Operator -> String
opToStr Plus = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div = "/"
opToStr Root = "v"
opToStr PRight = ")"
opToStr PLeft = "("

showToken :: Token -> String
showToken (TOperator op) = opToStr op
showToken TSpace = " "
showToken (TNumber nb) = show nb