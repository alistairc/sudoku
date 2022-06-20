module Main where

import ConsoleUi.ConsoleApp (runSudokuMain)
import System.Console.Haskeline
import Control.Monad.State
import Sudoku.Grid (Grid, emptyGrid)
import Data.Maybe
import ConsoleUi.GridState
import ConsoleUi.ConsoleIO

main :: IO ()
main = evalStateT (runInputT defaultSettings runSudokuMain) emptyGrid


--type alias for our monand transformer stack
type AppM = InputT (StateT Grid IO)

-- == instances ==
    
instance MonadConsole AppM where
  consoleWrite = outputStrLn

  consoleReadChar = fromMaybe ' ' <$> getInputChar noprompt
    where noprompt = ""


instance MonadGrid AppM where
  getGrid = lift get
  setGrid = lift . put
