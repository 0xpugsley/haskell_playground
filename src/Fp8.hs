module Fp8
  ( xand,
    xconcat,
    xreplicate,
    (!!!),
    xelem,
    merge,
    msort,
  )
where

xand :: [Bool] -> Bool
xand [] = True
xand (x : xs) = x && xand xs

xconcat :: [[a]] -> [a]
xconcat [] = []
xconcat (x : xs) = x ++ xconcat (xs)

xreplicate :: Int -> a -> [a]
xreplicate 0 _ = []
xreplicate n x = x : xreplicate (n -1) x

(!!!) :: [a] -> Int -> a
(!!!) [] _ = error ("empty list")
(!!!) (x : xs) 0 = x
(!!!) (x : xs) n = xs !!! (n - 1)

xelem :: Eq a => a -> [a] -> Bool
xelem _ [] = False
xelem e (x : xs) =
  (x == e) || xelem e xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xx) (msort yy)
  where
    (xx, yy) = split xs