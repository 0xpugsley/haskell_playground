module Fp7
  ( pyths,
    perfects,
    scalarProduct,
  )
where

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors x = [i | i <- [1 .. x], x `mod` i == 0]

-- factors x = [i | i <- [1 .. x], (x `mod` i == 0) && (i /= x)]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (init (factors x)) == x]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]