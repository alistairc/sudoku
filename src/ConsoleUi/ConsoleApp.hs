module ConsoleUi.ConsoleApp where

import Sudoku.Grid
import Sudoku.Rendering

-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()

-- called by main itself but not directly in IO, so as to allow testing with an alternative monad
runSudoku :: (MonadConsole m) => m ()
runSudoku = consoleWrite $ renderGrid emptyGrid

instance MonadConsole IO where
  consoleWrite = putStrLn