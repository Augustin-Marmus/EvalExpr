module Main where

import Evaluator
import System.Environment
import System.IO
import Text.Printf

main :: IO ()
main = do 
        args <- getArgs
        if args == []
          then do
            putStrLn "./funEvalExpr StringToCompute"
            return ()
          else do
            putStrLn $ (printf "%.2f") $ evalExpr $ head args
            return ()