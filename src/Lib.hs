module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = do
  testSafeTail
  testSafeTail

safeTailIf :: [a] -> [a]
safeTailIf x = if null x then [] else tail x

safeTailGuard :: [a] -> [a]
safeTailGuard x
  | null x = []
  | otherwise = tail x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail x = tail x

testSafeTail :: IO ()
testSafeTail = putStrLn $ "xxx " ++ show (safeTail [1, 2, 3])

(||) :: Bool -> Bool -> Bool
(||) False b = b
(||) True _ = True

(&&) :: Bool -> Bool -> Bool
(&&) a b =
  if a
    then b
    else False