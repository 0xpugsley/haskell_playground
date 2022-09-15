{-# LANGUAGE StrictData #-}
module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

data Prog
  = Assign Name Expr
  | If Expr Prog Prog
  | While Expr Prog
  | Seqn [Prog]
  deriving (Show)

data Expr
  = Val Int
  | Var Name
  | App Op Expr Expr
  deriving (Show)

type Name = Char

data Op = Add | Sub | Mul | Div deriving (Show, Eq)

type Stack = [Int]

type Mem = [(Name, Int)]

type Code = [Inst]

data Inst
  = PUSH Int
  | PUSHV Name
  | POP Name
  | DO Op
  | JUMP Label
  | JUMPZ Label
  | LABEL Label
  deriving (Show, Eq)

type Label = Int

program :: Prog
program =
  Seqn
    [ Assign 'A' (Val 1),
      Assign 'B' (Val 10),
      While
        (Var 'B')
        ( Seqn
            [ Assign 'A' (App Mul (Var 'A') (Var 'B')),
              Assign 'B' (App Sub (Var 'B') (Val 1))
            ]
        )
    ]

programIfBranchA :: Prog
programIfBranchA =
  Seqn
    [ Assign 'A' (Val 1),
      Assign 'B' (Val 10),
      If
        (Var 'A')
        (Assign 'B' (App Sub (Var 'B') (Val 1)))
        (Assign 'B' (App Add (Var 'B') (Val 1)))
    ]

programIfBranchB :: Prog
programIfBranchB =
  Seqn
    [ Assign 'A' (Val 0),
      Assign 'B' (Val 10),
      If
        (Var 'A')
        (Assign 'B' (App Sub (Var 'B') (Val 1)))
        (Assign 'B' (App Add (Var 'B') (Val 1)))
    ]

program2 :: Prog
program2 =
  Seqn
    [ Assign 'A' (Val 1),
      Assign 'B' (Val 10)
    ]

program3 :: Prog
program3 =
  Assign 'A' (Val 1)

code :: [Inst]
code =
  [ PUSH 1,
    POP 'A',
    PUSH 10,
    POP 'B',
    LABEL 0,
    PUSHV 'B',
    JUMPZ 1,
    PUSHV 'A',
    PUSHV 'B',
    DO Mul,
    POP 'A',
    PUSHV 'B',
    PUSH 1,
    DO Sub,
    POP 'B',
    JUMP 0,
    LABEL 1
  ]

nestedExpr :: Expr
nestedExpr = App Sub (App Add (Var 'A') (Var 'B')) (Val 1)

compileExpr :: Expr -> WriterT Code (State Int) ()
compileExpr (Val x) = tell [PUSH x]
compileExpr (Var name) = tell [PUSHV name]
compileExpr (App op expr1 expr2) =
  do
    compileExpr expr1
    compileExpr expr2
    tell [DO op]

compile :: Prog -> WriterT Code (State Int) ()
compile (Seqn []) = tell []
compile (If expr prog1 prog2) =
  do
    branchB <- get
    put (branchB + 1)
    endLabel <- get
    put (endLabel + 1)
    compileExpr expr
    tell [JUMPZ branchB]
    compile prog1
    tell [JUMP endLabel]
    tell [LABEL branchB]
    compile prog2
    tell [LABEL endLabel]
compile (Seqn (prog : progs)) =
  do
    compile prog
    compile (Seqn progs)
compile (Assign name expr) =
  do
    compileExpr expr
    tell [POP name]
compile (While expr prog) =
  do
    startLabel <- get
    put (startLabel + 1)
    endLabel <- get
    put (endLabel + 1)
    tell [LABEL startLabel]
    compileExpr expr
    tell [JUMPZ endLabel]
    compile prog
    tell [JUMP startLabel]
    tell [LABEL endLabel]

comp :: Prog -> Code
comp prog = snd (evalState (runWriterT (compile prog)) 0)

-- vm
modifyMem :: Name -> Int -> Mem -> Mem
modifyMem n v m = (n, v) : filter (\(k, _) -> k /= n) m

getFromMem :: Name -> Mem -> Int
getFromMem name mem = snd (head $ filter (\(k, _) -> k == name) mem)

pop2mem :: Name -> ReaderT Code (State (Stack, Mem)) ()
pop2mem name = state $ \(x : xs, mem) -> ((), (xs, modifyMem name x mem))

pop :: ReaderT Code (State (Stack, Mem)) Int
pop = state $ \(x : xs, mem) -> (x, (xs, mem))

pushV :: Name -> ReaderT Code (State (Stack, Mem)) ()
pushV name = state $ \(xs, mem) -> ((), (getFromMem name mem : xs, mem))

push :: Int -> ReaderT Code (State (Stack, Mem)) ()
push v = state $ \(xs, mem) -> ((), (v : xs, mem))

exec' :: Code -> ReaderT Code (State (Stack, Mem)) ()
exec' [] = do
  return ()
exec' ((PUSH x) : xs) = do
  push x
  exec' xs
exec' ((PUSHV name) : xs) = do
  pushV name
  exec' xs
exec' ((POP name) : xs) = do
  pop2mem name
  exec' xs
exec' ((DO op) : xs) = do
  y <- pop
  x <- pop
  case op of
    Add -> push (x + y)
    Sub -> push (x - y)
    Mul -> push (x * y)
    Div -> push (x `div` y)
  exec' xs
exec' ((JUMP label) : _) = do
  c <- ask
  exec' (dropWhile (/= LABEL label) c)
exec' ((JUMPZ label) : xs) = do
  x <- pop
  if x == 0
    then do
      c <- ask
      exec' (dropWhile (/= LABEL label) c)
    else exec' xs
exec' ((LABEL _) : xs) = do
  exec' xs

exec :: Code -> (Stack, Mem)
exec c = execState (runReaderT (exec' c) c) ([], [])

test_mem :: [(Char, Integer)]
test_mem = [('A', 3628800), ('B', 0)]

main :: IO ()
main = do
  putStrLn "brrrr"