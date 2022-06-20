module ConsoleUi.ConsoleIO where

-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()
  consoleReadChar :: m Char

