module Main where

import ConsoleUi.ConsoleApp (runSudokuMain)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings runSudokuMain
