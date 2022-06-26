module Main where

import Fp7 (perfects, pyths, scalarProduct)
import Lib (someFunc)

main :: IO ()
main = do
  print "------"
  print (pyths 5)
  print (perfects 500)
  print (scalarProduct [1, 2, 3] [1, 2, 3])
