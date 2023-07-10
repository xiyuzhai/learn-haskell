module Main where

type Stream a = [a]

attention :: Stream q -> Stream (q -> Bool) -> Stream v -> Stream [v]
attention qs ks vs = map (\q -> [v | (k, v) <- zip ks vs, k q]) qs

-- type Q = [Int]  -- substitute Int with the actual type
-- type K = [Int]  -- substitute Int with the actual type
-- type V = [Int]  -- substitute Int with the actual type
-- attention :: (Q, K, V) -> [V]
-- attention (qs, ks, vs) = map (\q -> [v | (k, v) <- zip ks vs, q == k]) qs
main :: IO ()
main = putStrLn "Hello, Haskell!"
