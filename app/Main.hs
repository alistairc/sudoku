module Main where

import ConsoleUi.ConsoleApp (runSudokuMain)
import System.Console.Haskeline
import Control.Monad.State
import Sudoku.Grid (emptyGrid)

main :: IO ()
main = evalStateT (runInputT defaultSettings runSudokuMain) emptyGrid
