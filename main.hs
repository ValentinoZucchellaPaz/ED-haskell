module Main where

import Data.List (intersperse)

main :: IO ()
main = do
  putStrLn "Hello, world!"
  print (intersperse '.' "Haskell")

-- clase 1 y 2: aritmetica y haskell
-- clase 3 y 4: funciones y recursividad
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)