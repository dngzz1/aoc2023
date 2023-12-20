module Main where

import Data.ByteString qualified as B
import Data.Char (isDigit)
import Data.Text (pack, replace, unpack)
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = String -- default to Bytestring, but very likely you'll need to change it

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser = id

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . map extractDigits . lines

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = sum . calibratedValues

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

extractDigits :: String -> Int
extractDigits string = parseInt . headAndLast $ filter isDigit string

headAndLast :: String -> String
headAndLast d = [head d, last d]

parseInt :: String -> Int
parseInt d = read d :: Int

digitsDict :: [(String, String)]
digitsDict =
  [ ("one", "one1one"),
    ("two", "two2two"),
    ("three", "three3three"),
    ("four", "four4four"),
    ("five", "five5five"),
    ("six", "six6six"),
    ("seven", "seven7seven"),
    ("eight", "eight8eight"),
    ("nine", "nine9nine")
  ]

calibratedValues :: String -> [Int]
calibratedValues = map calibratedValue . lines

calibratedValue :: String -> Int
calibratedValue = extractDigits . replacedString

replacedString :: String -> String
replacedString = unpack . replaceAllWords . pack
  where
    replaceAllWords = composeAll $ map replaceWord digitsDict
    replaceWord pair = replace (pack (fst pair)) (pack (snd pair))

composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id