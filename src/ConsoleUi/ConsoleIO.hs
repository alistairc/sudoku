module ConsoleUi.ConsoleIO where

import System.Console.Haskeline
import Data.Maybe
import Control.Monad.State
import Sudoku.Grid


-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()
  consoleReadChar :: m Char

instance MonadConsole (InputT IO) where
  consoleWrite = outputStrLn

  consoleReadChar = fromMaybe ' ' <$> getInputChar noprompt
    where noprompt = ""

instance MonadConsole (StateT Grid (InputT IO)) where
  consoleWrite = lift . outputStrLn

  consoleReadChar = lift (fromMaybe ' ' <$> getInputChar noprompt)
    where noprompt = ""


