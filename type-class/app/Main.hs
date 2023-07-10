module Main where

import           Data.Default

data Person = Person { name :: String, age :: Int, email :: String }
  deriving (Show)

instance Default Person where
  def = Person { name = "John Doe", age = 30, email = "" }

main :: IO ()
main = do
  let defaultPerson = [def :: Person]
  putStrLn "Hello, Haskell!"
