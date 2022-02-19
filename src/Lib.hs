module Lib where

import Data.Function ((&))
import Data.List.Split

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq)

newtype Grid = Grid [Maybe Digit]
  deriving (Show, Eq)

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Eq, Ord)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Show, Eq, Ord)

type GridCoord = (Column, Row)

emptyGrid :: Grid
emptyGrid = Grid (replicate 81 Nothing)

renderSquare :: Maybe Digit -> Char
renderSquare Nothing = '.'
renderSquare (Just d) = case d of
  D1 -> '1'
  D2 -> '2'
  D3 -> '3'
  D4 -> '4'
  D5 -> '5'
  D6 -> '6'
  D7 -> '7'
  D8 -> '8'
  D9 -> '9'

renderGrid :: Grid -> String
renderGrid (Grid squares) =
  concatMap (\line -> renderLine line ++ ['\n']) (chunksOf 9 squares)

renderLine :: [Maybe Digit] -> String
renderLine = map renderSquare

parseGrid :: String -> Maybe Grid
parseGrid text = if length text /= 90 then Nothing else Just emptyGrid

moveAt :: GridCoord -> Digit -> Grid -> Grid
moveAt (x, y) digit initial =
  let (Grid squares) = initial
      index = (rowIndex y - 1) * 9 + (columIndex x - 1)
   in Grid $ take index squares ++ [Just digit] ++ drop (index + 1) squares

columIndex :: Column -> Int
columIndex col = case col of
  C1 -> 1
  C2 -> 2
  C3 -> 3
  C4 -> 4
  C5 -> 5
  C6 -> 6
  C7 -> 7
  C8 -> 8
  C9 -> 9

rowIndex :: Row -> Int
rowIndex row = case row of
  R1 -> 1
  R2 -> 2
  R3 -> 3
  R4 -> 4
  R5 -> 5
  R6 -> 6
  R7 -> 7
  R8 -> 8
  R9 -> 9
