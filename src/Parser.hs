module Parser (Expression(..), parse) where

import Tokenizer
{- 

Expression  <- Term [+-] Expression
             | Term

Term        <- Factor [*/] Term
             | Factor

Factor      <- Power [^] Factor
             | [v] Factor
             | Power

Power       <- Number
             | [-+] Power
             | '(' Expression ')'

-}

data Expression = SumNode Operator Expression Expression
                | ProdNode Operator Expression Expression
                | PowNode Operator Expression Expression
                | UnaryNode Operator Expression
                | NumNode Double 
                deriving (Show)

power :: [Token] -> (Expression, [Token])
power ((TNumber nb):t) = ((NumNode (fromIntegral nb)), t)
power ((TOperator op):t) | elem op [Plus, Minus] = let (tree , rest) = power t in ((UnaryNode op tree), rest)
                         | op == PLeft = let (tree, rest) = expression t in
                                            case rest of
                                              ((TOperator op):rest) | op == PRight -> (tree, rest)
                                              otherwise -> error $ "Mismatching parenthesis"
power t | otherwise = error $ "Empty Tokens"

factor :: [Token] -> (Expression, [Token])
factor ((TOperator op):t) | op == Root = let (tree, rest) = factor t in ((PowNode op tree (NumNode 0.5)), rest)
factor t = let (leftTree, rest) = power t in
              case rest of 
                ((TOperator op):t) | op == Power -> let (rightTree, rest') = factor t in ((PowNode op leftTree rightTree), rest')
                otherwise                        -> (leftTree, rest)


term :: [Token] -> (Expression, [Token])
term t = let (leftTree, rest) = factor t in
            case rest of 
              ((TOperator op):t) | elem op [Times, Div] -> let (rightTree, rest') = term t in ((ProdNode op leftTree rightTree), rest')
              otherwise                                 -> (leftTree, rest)

expression :: [Token] -> (Expression, [Token])
expression t = let (leftTree, rest) = term t in
                  case rest of
                    ((TOperator op):t) | elem op [Plus, Minus] -> let (rightTree, rest') = expression t in ((SumNode op leftTree rightTree), rest')
                    otherwise                                  -> (leftTree, rest)


parse :: [Token] -> Expression
parse t = let (tree, tokens) = expression t in
            if null tokens
              then tree
              else error $ "Syntax Error: " ++ show tokens