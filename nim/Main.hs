module Main where

import Data.Char
import System.IO

type Board = [Int]

data Player = PlayerA | PlayerB deriving (Show)

newLine :: IO ()
newLine = putStr "\n"

getDigit :: String -> IO Int
getDigit prompt = do
  putStrLn prompt
  digit <- getLine
  newLine
  if isDigit (head digit)
    then return (digitToInt (head digit))
    else do
      newLine
      putStrLn "ERROR: Invalid input"
      getDigit prompt

turn :: Player -> IO (Int, Int)
turn player = do
  print player
  newLine
  row <- getDigit "Enter row:"
  newLine
  num <- getDigit "Enter number of stars:"
  return (row, num)

stars :: Int -> String
stars x = concat (replicate x "* ")

printBoardRow :: Int -> Int -> IO ()
printBoardRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (stars num)

printBoard :: Int -> Board -> IO ()
printBoard _ [] = newLine
printBoard index (x : xs) = do
  printBoardRow index x
  printBoard (index + 1) xs

finished :: Board -> Bool
finished = all (== 0)

valid :: Int -> Int -> Board -> Bool
valid row num board = board !! (row - 1) >= num

changeBoard :: Int -> Int -> Board -> Board
changeBoard r n stars = [adjust x y | (x, y) <- zip stars [1 ..]]
  where
    adjust x y = if y == r then x - n else x

flipPlayer :: Player -> Player
flipPlayer PlayerA = PlayerB
flipPlayer PlayerB = PlayerA

game :: Board -> Player -> IO ()
game stars player = do
  printBoard 1 stars
  if finished stars
    then do
      putStr (show (flipPlayer player))
      putStrLn " won!"
    else do
      (row, num) <- turn player
      if valid row num stars
        then game (changeBoard row num stars) (flipPlayer player)
        else do
          newLine
          putStrLn "Error: Invalid move!"
          game stars player

main :: IO ()
main = do
  game [5, 4, 3, 2, 1] PlayerA
