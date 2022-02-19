module Sudoku.Rendering
  ( renderGrid,
    renderSquare,
  )
where

import Data.List.Split
import Sudoku.Grid

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
