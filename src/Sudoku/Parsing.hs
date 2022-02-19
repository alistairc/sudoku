module Sudoku.Parsing
  ( parseGrid,
  )
where

import Sudoku.Grid

parseGrid :: String -> Maybe Grid
parseGrid text = if length text /= 90 then Nothing else Just emptyGrid
