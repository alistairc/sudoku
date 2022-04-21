module Sudoku.Grid
  ( Grid,
    Digit (..),
    Column (..),
    Row (..),
    GridCoord,
    Group (..),

    emptyGrid,
    getCell,
    moveAt,
    gridToList,
    listToGrid,
    emptyGroup,
    groupFromList,
    groupToList,
    missingFromGroup,
    selectRow
  )
where

import Data.Function ((&))
import Data.List (intersect)
import Data.Maybe (catMaybes)

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq, Enum, Bounded)

newtype Grid = Grid [Maybe Digit]
  deriving (Show, Eq)

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Eq, Ord, Enum, Bounded)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Show, Eq, Ord, Enum, Bounded)

data Group = Group
    (Maybe Digit) (Maybe Digit) (Maybe Digit)
    (Maybe Digit) (Maybe Digit) (Maybe Digit)
    (Maybe Digit) (Maybe Digit) (Maybe Digit)
  deriving (Show, Eq)

type GridCoord = (Column, Row)

emptyGrid :: Grid
emptyGrid = Grid (replicate 81 Nothing)

listToGrid :: [Maybe Digit] -> Grid
listToGrid list = Grid $ take 81 (list ++ replicate 81 Nothing)

gridToList :: Grid -> [Maybe Digit]
gridToList (Grid cells) = cells

moveAt :: GridCoord -> Digit -> Grid -> Grid
moveAt (x, y) digit initial =
  let (Grid cells) = initial
      index = gridIndex (x,y)
   in Grid $ take index cells ++ [Just digit] ++ drop (index + 1) cells

getCell :: GridCoord -> Grid -> Maybe Digit
getCell (x,y) grid = gridToList grid !! gridIndex (x,y)

gridIndex :: GridCoord -> Int
gridIndex (x,y) = (rowIndex y - 1) * 9 + (columnIndex x - 1)

columnIndex :: Column -> Int
columnIndex col = fromEnum col + 1

rowIndex :: Row -> Int
rowIndex row = fromEnum row + 1

emptyGroup :: Group
emptyGroup = Group
  Nothing Nothing Nothing
  Nothing Nothing Nothing
  Nothing Nothing Nothing

groupFromList :: [Maybe Digit] -> Group
groupFromList [] = emptyGroup
groupFromList [d1] = Group d1 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
groupFromList [d1,d2] = Group d1 d2 Nothing Nothing Nothing Nothing Nothing Nothing Nothing
groupFromList [d1,d2,d3] = Group d1 d2 d3 Nothing Nothing Nothing Nothing Nothing Nothing
groupFromList [d1,d2,d3,d4] = Group d1 d2 d3 d4 Nothing Nothing Nothing Nothing Nothing
groupFromList [d1,d2,d3,d4,d5] = Group d1 d2 d3 d4 d5 Nothing Nothing Nothing Nothing
groupFromList [d1,d2,d3,d4,d5,d6] = Group d1 d2 d3 d4 d5 d6 Nothing Nothing Nothing
groupFromList [d1,d2,d3,d4,d5,d6,d7] = Group d1 d2 d3 d4 d5 d6 d7 Nothing Nothing
groupFromList [d1,d2,d3,d4,d5,d6,d7,d8] = Group d1 d2 d3 d4 d5 d6 d7 d8 Nothing
groupFromList [d1,d2,d3,d4,d5,d6,d7,d8,d9] = Group d1 d2 d3 d4 d5 d6 d7 d8 d9
groupFromList (d1:d2:d3:d4:d5:d6:d7:d8:d9:_) = Group d1 d2 d3 d4 d5 d6 d7 d8 d9

groupToList :: Group -> [Maybe Digit]
groupToList (Group d1 d2 d3 d4 d5 d6 d7 d8 d9) =
   [d1,d2,d3,d4,d5,d6,d7,d8,d9]

missingFromGroup :: Group -> [Digit]
missingFromGroup grp =
  allDigits & filter notFound
  where
    notFound x  = x `notElem` foundDigits
    foundDigits = groupToList grp & catMaybes
    allDigits = enumFrom (minBound :: Digit)

selectRow :: Row -> Grid -> Group
selectRow row (Grid digits) =
  groupFromList $ selectRange startIndex endIndex digits
  where
    startIndex = (rowNum - 1) * 9
    endIndex = (rowNum * 9) - 1
    rowNum = rowIndex row
    selectRange minIndex maxIndex list = list & indexList & filter (between minIndex maxIndex . snd) & map fst
    indexList list = zip list ([0 ..] :: [Int])
    between min max x = (x >= min) && (x <= max)