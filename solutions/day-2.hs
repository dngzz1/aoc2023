module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString.Char8 qualified as B
import Data.Either (partitionEithers)
import Data.Foldable (Foldable (foldl'))
import Data.Functor (($>))
import Data.Map qualified as Map
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
data Color = Red | Green | Blue deriving (Show, Eq, Ord)

type GameId = Int

type Input = [(GameId, [Map.Map Color Int])]

type Solution = Int

parserLine :: Atto.Parser (GameId, [Map.Map Color Int])
parserLine = (,) <$> parseGameId <*> parseMaps
  where
    parseGameId = Atto.string "Game " *> Atto.decimal <* Atto.string ": "
    parseMaps = parseMap `Atto.sepBy` Atto.string "; "
    parseMap = Map.fromList <$> (parseKeyVal `Atto.sepBy` Atto.string ", ")
    parseKeyVal = do
      amount <- Atto.decimal <* Atto.space
      col <- parseColour
      pure (col, amount)
    parseColour =
      Atto.string "red" $> Red
        <|> Atto.string "green" $> Green
        <|> Atto.string "blue" $> Blue

parser :: Atto.Parser Input
parser = parserLine `Atto.sepBy` Atto.endOfLine

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . map fst

helper1 :: Input -> Input
helper1 = id

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  [part, filepath] <- getArgs
  bs <- B.readFile filepath
  let input =
        case Atto.parseOnly parser bs of
          Right i -> i
          Left err -> error err
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ helper1 input
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
