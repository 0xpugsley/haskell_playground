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
stringifyRow = foldr (\a b -> show(a) ++ b) ""

stringifyBoard :: Board -> String
stringifyBoard b = unlines ([stringifyRow r | r <- b]) ++ replicate cols '-' ++ "\n" ++ filter (/= ' ') (unwords (map show [0 .. cols -1]))

dummyRow :: Row
dummyRow = [B, O, B, X]

createEmptyBoard :: Board
createEmptyBoard = replicate rows createRow
  where
    createRow = replicate cols B

createTestBoard :: Board
createTestBoard = [[B, B, X], [B, X, O], [O, O, X]]

createTestBoard1 :: Board
createTestBoard1 = [[B, B, B], [B, X, O], [O, O, X]]

data Tree a = Node a [Tree a] deriving (Show)

columns :: Board -> Board
columns = transpose

choice :: Player -> Row -> Row
choice p r = drop 1 (takeWhile (== B) r) ++ [p] ++ dropWhile (== B) r

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

ll = [[X, O, X, X], [X, X, O, O], [X, O, X, O]]

lll = [[X, O, X, O], [X, O, X, O], [O, X, O, X]]

a = [[B, B, X], [B, X, O], [O, O, X]]

badX = [[X, O, X], [X, X, O], [O, O, X]]

badB = [[O, X, X], [X, X, O], [O, O, X]]

shift :: Int -> Int -> [Player] -> [Player]
shift n len l = replicate (len - n) B ++ l ++ replicate n B

-- shift n len l = take (length l) (drop n (cycle l))

shiftedFromTop :: Board -> Board
shiftedFromTop b = [shift i len r | (r, i) <- zip b [0 .. length b]]
  where
    len = length b - 1

shiftedFromBottom :: Board -> Board
shiftedFromBottom b = [shift i len r | (r, i) <- zip b (reverse [0 .. length b])]
  where
    len = length b - 1

-- idk maybe can be cut earlier
label :: Board -> Player
label = foldr (\r p -> if p == B then winner r else p) B

whoIsWinner :: Board -> Player
whoIsWinner b = case filter (/= B) [rows, cols, diagonal, contrDiagonal] of
  [x] -> x
  (x : xs) -> if all (== x) xs then x else B
  [] -> B
  where
    rows = label b
    cols = label (columns b)
    diagonal = label (columns (shiftedFromTop b))
    contrDiagonal = label (columns (shiftedFromBottom b))

createChildrenRaw :: Player -> Board -> [Board]
createChildrenRaw p b = map (\(r, i) -> replace i r b) (filter (\(e, i) -> B `elem` e) (zip b [0 .. length b]))
  where
    replace i r b' = first ++ choice p r : drop 1 second
      where
        (first, second) = splitAt i b'

createChildren :: Player -> Board -> [Board]
createChildren p b = map columns (createChildrenRaw p (columns b))

createGameTreeRaw :: Player -> Int -> Board -> Tree (Board, Player)
createGameTreeRaw p 1 board = Node (board, p) []
createGameTreeRaw player depth' board = Node (board, player) (map (createGameTreeRaw (next player) (depth' - 1)) (createChildren player board))

createGameTree :: Player -> Board -> Tree (Board, Player)
createGameTree player = createGameTreeRaw player depth

labelTree :: Tree (Board, Player) -> Tree (Board, Player, Player)
labelTree (Node (b, turn) []) = Node (b, turn, whoIsWinner b) []
labelTree (Node (b, turn) l) = Node (b, turn, B) (map labelTree l)

-- 1. Produce the game tree to specified depth
-- 2. Label each leaf with winner, or B if not over or draw
-- 3. Then work up the tree - if O to play, take min of the children (O<B<X)
-- if X to play, take max of children
-- 4 Best move is one with same label as root.

untree :: [Tree (Board, Player, Player)] -> [Player]
untree = map (\(Node (_, _, l) _) -> l)

testNode :: Tree ([[Player]], Player)
testNode = Node ([[B, B, X], [B, X, O], [O, O, X]], B) [Node ([[B, B, X], [B, X, O], [O, O, X]], X) [Node ([[B, B, X], [B, X, O], [O, O, X]], X) []]]

testBoard2 = [[B, X, X], [B, X, O], [O, O, X]]

testBoard3 = [[B, X, X], [X, X, O], [O, O, X]]

workUpTheTree :: Tree (Board, Player, Player) -> Tree (Board, Player, Player)
workUpTheTree (Node (b, t, l) []) = Node (b, t, l) []
workUpTheTree (Node (b, t, l) xs) = Node (b, t, compute xs') xs'
  where
    xs' = map workUpTheTree xs
    compute = (if t == O then minimum else maximum) . untree

getBestBoard :: Tree (Board, Player, Player) -> Board
getBestBoard (Node (b, _, l) xs) = fst (head (filter (\a -> snd a == l) xs'))
  where
    xs' = [(b', l') | (Node (b', _, l') _) <- xs]

makeMove :: Int -> Player -> Board -> Board
makeMove position player board = columns (map (\(r, i) -> if i == position then (modify r) else r) (zip (columns board) [0 ..]))
  where
    modify r = drop 1 (takeWhile (== B) r) ++ [player] ++ dropWhile (== B) r

game :: Player -> Board -> IO ()
game player board = do
  putStrLn (stringifyBoard board)
  input <- getLine
  let x = (read input :: Int)
  let newBoard = makeMove x player board
  putStrLn (stringifyBoard newBoard)
  putStr "\n"
  let nextMove = getBestBoard (workUpTheTree (labelTree (createGameTree (next player) newBoard)))
  -- putStrLn (stringifyBoard nextMove)
  game player nextMove

main :: IO ()
main = do
  putStrLn "hey"
  game X createEmptyBoard

-- print (createGameTree createTestBoard O)
