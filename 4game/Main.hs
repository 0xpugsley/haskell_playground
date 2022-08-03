module Main where

import Data.Char
import Data.List
import Data.String
import System.IO
import System.IO.Unsafe
import System.Random hiding (next)
import Text.ParserCombinators.ReadP (count)

-- rows :: Int
-- rows = 6

-- cols :: Int
-- cols = 7

-- win :: Int
-- win = 4

-- depth :: Int
-- depth = 6

rows :: Int
rows = 3

cols :: Int
cols = 3

win :: Int
win = 3

depth :: Int
depth = 4

type Board = [Row]

type Row = [Player]

data Player = O | B | X deriving (Eq, Ord)

instance Show Player where
  show O = "O"
  show B = "."
  show X = "X"

-- instance Show Board where
-- show b = []

stringifyRow :: Row -> String
stringifyRow r = unwords [show a | a <- r]

stringifyBoard :: Board -> String
stringifyBoard b = unlines ([stringifyRow r | r <- b]) ++ unwords (map show [0 .. cols -1])

dummyRow :: Row
dummyRow = [B, O, B, X]

createEmptyBoard :: Board
createEmptyBoard = replicate rows createRow
  where
    createRow = replicate cols B

createTestBoard :: Board
createTestBoard = [[B, B, X], [B, X, O], [O, O, X]]

-- 1. Produce the game tree to specified depth
-- 2. Label each leaf with winner, or B if not over or draw
-- 3. Then work up the tree - if O to play, take min of the children (O<B<X)
-- if X to play, take max of children
-- 4 Best move is one with same label as root.

data Tree a = Node a [Tree a] deriving (Show)

columns :: Board -> Board
columns = transpose

choice :: Player -> Row -> Row
choice p r = drop 1 (takeWhile (== B) r) ++ [p] ++ dropWhile (== B) r

createChildrenRaw :: Player -> Board -> [Board]
createChildrenRaw p b = map (\(r, i) -> replace i r b) (filter (\(e, i) -> B `elem` e) (zip b [0 .. length b]))
  where
    replace i r b' = first ++ choice p r : drop 1 second
      where
        (first, second) = splitAt i b'

createChildren :: Player -> Board -> [Board]
createChildren p b = map columns (createChildrenRaw p (columns b))

next :: Player -> Player
next X = O
next O = X
next _ = B

-- diagonal :: Board -> Row

winner :: Row -> Player
winner r =
  case groups of
    (O : _) -> O
    (X : _) -> X
    _ -> B
  where
    groups = [head i | i <- group r, head i /= B, length i >= win]

l :: Board
l = [[O, B, O], [X, X, X], [B, X, B]]

label :: Board -> Player
label = foldr (\r p -> if p == B then winner r else p) B

createGameTreeRaw :: Player -> Int -> Board -> Tree Board
createGameTreeRaw _ 0 board = Node board []
createGameTreeRaw player depth' board = Node board (map (createGameTreeRaw (next player) (depth' - 1)) (createChildren player board))

createGameTree :: Player -> Board -> Tree Board
createGameTree player = createGameTreeRaw player depth

main :: IO ()
main = do
  putStrLn "hey"
  -- putStrLn (stringifyBoard createEmptyBoard)
  putStrLn (stringifyBoard createTestBoard)

-- print (createGameTree createTestBoard O)
