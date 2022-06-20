module ConsoleUi.GridState where

import Sudoku.Grid (Grid)
import Control.Monad.State

class Monad m => MonadGrid m where
  getGrid :: m Grid
  setGrid :: Grid -> m ()

updateGrid :: MonadGrid m => (Grid -> Grid) -> m ()
updateGrid f = getGrid >>= setGrid . f
