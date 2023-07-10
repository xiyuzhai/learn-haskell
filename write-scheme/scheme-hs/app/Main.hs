module Main (main) where

import           System.Environment
import           Lib

main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn args
  putStrLn (readExpr (head args))
