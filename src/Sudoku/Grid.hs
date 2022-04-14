module Sudoku.Grid
  ( Grid,
    Digit (..),
    Column (..),
    Row (..),
    GridCoord,
    DigitSet (..),

    emptyGrid,
    getSquare,
    moveAt,
    gridToList,
    listToGrid,
    emptyDigitSet,
    digitSetFromList,
    digitSetToList,
    selectRow
  )
where

import Data.Function ((&))

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq)

newtype Grid = Grid [Maybe Digit]
  deriving (Show, Eq)

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Eq, Ord, Enum)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Show, Eq, Ord, Enum)

newtype DigitSet = DigitSet [Maybe Digit]
  deriving (Show, Eq)

type GridCoord = (Column, Row)

emptyGrid :: Grid
emptyGrid = Grid (replicate 81 Nothing)

listToGrid :: [Maybe Digit] -> Grid
listToGrid list = Grid $ take 81 (list ++ replicate 81 Nothing)

gridToList :: Grid -> [Maybe Digit]
gridToList (Grid squares) = squares

moveAt :: GridCoord -> Digit -> Grid -> Grid
moveAt (x, y) digit initial =
  let (Grid squares) = initial
      index = gridIndex (x,y)
   in Grid $ take index squares ++ [Just digit] ++ drop (index + 1) squares

getSquare :: GridCoord -> Grid -> Maybe Digit
getSquare (x,y) grid = gridToList grid !! gridIndex (x,y)

gridIndex :: GridCoord -> Int
gridIndex (x,y) = (rowIndex y - 1) * 9 + (columnIndex x - 1)

columnIndex :: Column -> Int
columnIndex col = fromEnum col + 1

rowIndex :: Row -> Int
rowIndex row = fromEnum row + 1

emptyDigitSet :: DigitSet
emptyDigitSet = DigitSet (replicate 9 Nothing)

digitSetFromList :: [Maybe Digit] -> DigitSet
digitSetFromList list = DigitSet $ take 9 $ list ++ repeat Nothing

digitSetToList :: DigitSet -> [Maybe Digit]
digitSetToList (DigitSet list) = list

selectRow :: Row -> Grid -> DigitSet
selectRow row (Grid digits) =
  DigitSet $ selectRange startIndex endIndex digits
  where
    startIndex = (rowNum - 1) * 9
    endIndex = (rowNum * 9) - 1
    rowNum = rowIndex row
    selectRange minIndex maxIndex list = list & indexList & filter (between minIndex maxIndex . snd) & map fst
    indexList list = zip list ([0 ..] :: [Int])
    between min max x = (x >= min) && (x <= max)