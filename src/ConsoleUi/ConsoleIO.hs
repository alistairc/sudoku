module ConsoleUi.ConsoleIO where

import System.Console.Haskeline
import Data.Maybe


-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()
  consoleReadChar :: m Char

instance MonadConsole (InputT IO) where
  consoleWrite = outputStrLn

  consoleReadChar = fromMaybe ' ' <$> getInputChar noprompt
    where noprompt = ""
