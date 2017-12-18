module Main where

import Prelude
import Data.Char
import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do 
        expr <- getArgs
        if expr == []
          then
            return ()
          else do
            putStrLn $ evalExpr $ epurStr $ head expr

epurStr :: String -> String
epurStr = filter (/= ' ')

evalExpr :: String -> String
evalExpr s = intercalate "|" $ tokenize s

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Nbr Double

eval :: [String] -> Expr
eval [] = error "Unexpected end of List"
eval (h:rest)
  | all isDigit h = Nbr (read s::Double)
  | h == '(' = eval

npi ::[String] -> [String]
npi s = _npi s []

_npi :: [String] -> [String] -> [String]
_npi [] acc = reverse acc
_npi (h:rest) acc
  | all isDigit h = (_npi rest acc) ++ h
  | h == '(' = _npi rest $ acc ++ [h]
-- eval :: Expr -> Integer
-- eval e = case e of 
--   Add a b -> eval a + eval b
--   Sub a b -> eval a - eval b
--   Mul a b -> eval a * eval b
--   Div a b -> eval a `div` eval b
--   Pow a b -> eval a ^ eval b
--   Neg a   -> 0 - eval a
--   Sqr a   -> floor . sqrt . fromIntegral . eval e
--   otherwise -> e

tokenize :: String -> [String]
tokenize s = _tokenize s ""

_tokenize :: String -> String -> [String]
_tokenize [] acc = [acc]
_tokenize (h:rest) acc 
  | elem h "+-*/()v^" = acc : [h] : (_tokenize rest "")
  | isDigit h = _tokenize rest $ acc ++ [h]
  | otherwise = error $ "Bad input" ++ [h]

-- expr :: [String] -> (Expr,[String])
-- expr [] = error "unexpected end of input"
-- expr (s:ss) 
--   | all isDigit s = (E_Int (read s),ss)
--   | s == "-" = let (e,ss') = expr ss in (E_Neg e,ss') 
--   | s == "+" = (E_Sum e e',ss'')
--   where 
--     (e,ss') = expr ss
--     (e',ss'') = expr ss'
--             -- more cases