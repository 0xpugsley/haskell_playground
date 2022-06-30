import Fp12 (Nat (Succ, Zero), add, multiply)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

testZeroCase = TestCase (assertEqual "add" (Succ (Succ Zero)) (add (Succ (Succ Zero)) Zero))

testMultiplyZeroCase = TestCase (assertEqual "multiply 0" Zero (multiply (Succ (Succ Zero)) Zero))

testMultiplyOneCase = TestCase (assertEqual "multiply 1" (Succ (Succ Zero)) (multiply (Succ (Succ Zero)) (Succ Zero)))

testMultiplyCase =
  TestCase
    ( assertEqual
        "multiply 2 * 2"
        (Succ (Succ (Succ (Succ Zero))))
        (multiply (Succ (Succ Zero)) (Succ (Succ Zero)))
    )

-- testZeroCase = TestCase (print (add (Succ (Succ Zero)) Zero))

main :: IO ()
main = do
  counts <-
    runTestTT
      ( test
          [ testZeroCase,
            testMultiplyZeroCase,
            testMultiplyOneCase,
            testMultiplyCase
          ]
      )
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure