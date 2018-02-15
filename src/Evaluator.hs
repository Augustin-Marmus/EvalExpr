module Evaluator (evalExpr) where

import Tokenizer
import Parser

evaluate :: Expression -> Double
evaluate (SumNode op lExpr rExpr)  | op == Plus   = (evaluate lExpr) + (evaluate rExpr)
                                   | op == Minus  = (evaluate lExpr) - (evaluate rExpr)
evaluate (UnaryNode op expr)       | op == Plus   = (evaluate expr)
                                   | op == Minus  =  0 - (evaluate expr)
evaluate (PowNode op lExpr rExpr)  | op == Power  = (evaluate lExpr) ** (evaluate rExpr)
                                   | op == Root   = let lValue = evaluate lExpr in 
                                                      case lValue of
                                                        lValue | lValue >= 0.0 -> lValue ** (evaluate rExpr)
                                                        otherwise -> error $ "Square Root of a negative number is not defined for Reals"
evaluate (ProdNode op lExpr rExpr) | op == Times  = (evaluate lExpr) * (evaluate rExpr)
                                   | op == Div    = let rValue = evaluate rExpr in 
                                                      case rValue of 
                                                        rValue | rValue /= 0.0 -> (evaluate lExpr) / rValue
                                                        otherwise -> error $ "Division by zero"
evaluate (NumNode nb) = nb

evalExpr :: String -> Double
evalExpr = evaluate . parse . tokenize