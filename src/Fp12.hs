module Fp12
  ( add,
    multiply,
    Nat (Zero, Succ),
    Expr (Val, Add, Mul),
  )
where

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

multiply :: Nat -> Nat -> Nat
multiply Zero _ = Zero
multiply (Succ n) m = add (multiply n m) m

data Expr
  = Val Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

evalOne :: Expr -> Int
evalOne (Val n) = n
evalOne (Add x y) = evalOne x + evalOne y
evalOne (Mul x y) = evalOne x * evalOne y

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)
folde f g (Mul x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

data Tree a = Leaf a | Node (Tree a) (Tree a)