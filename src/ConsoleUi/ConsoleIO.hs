module ConsoleUi.ConsoleIO where

import System.Console.Haskeline
import Data.Maybe
import Sudoku.Grid (Grid, emptyGrid)
import Control.Monad.State


--type alias for our monand transformer stack
type AppM = InputT (StateT Grid IO)

-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()
  consoleReadChar :: m Char

instance MonadConsole AppM where
  consoleWrite = outputStrLn

  consoleReadChar = fromMaybe ' ' <$> getInputChar noprompt
    where noprompt = ""


class Monad m => MonadGrid m where
  getGrid :: m Grid
  setGrid :: Grid -> m ()

instance MonadGrid AppM where
  getGrid = lift get
  setGrid = lift . put

updateGrid :: MonadGrid m => (Grid -> Grid) -> m ()
updateGrid f = getGrid >>= setGrid . f
