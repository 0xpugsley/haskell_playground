module Main where

import Fp7 (perfects, pyths, scalarProduct)
import Fp8 (merge, msort, xand, xconcat, xelem, xreplicate, (!!!))
import Lib (someFunc)

main :: IO ()
main = do
  print "------"
  print (pyths 5)
  print (perfects 500)
  print (scalarProduct [1, 2, 3] [1, 2, 3])
  putStr "Fp8\n"
  print (xand [True, True, False])
  print (xand [True, True, True])
  print (xconcat [[1, 2, 3], [4, 5, 6]])
  print (xreplicate 5 1)
  print ([1, 2, 3, 4, 5] !!! 4)
  print ([1, 2, 3, 4, 5] !!! 0)
  print ([1, 2, 3, 4, 5] !!! 1)
  print (xelem 1 [1, 2, 3, 4, 5])
  print (xelem 10 [1, 2, 3, 4, 5])
  print (merge [2, 5, 6] [1, 3, 4])
  print (msort [5, 4, 3, 2, 1])
  print (msort [5, 1, 3, 2, 4])
