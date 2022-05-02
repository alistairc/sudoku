module Main where

import ConsoleUi.ConsoleApp (runSudokuMain, experimentalMain)
import System.Console.Haskeline

main :: IO ()
--main = runInputT defaultSettings runSudokuMain

main = experimentalMain
