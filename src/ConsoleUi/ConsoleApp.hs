module ConsoleUi.ConsoleApp where

import Sudoku.Grid
import Sudoku.Rendering

-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()
  consoleReadLine :: m String

-- called by main itself but not directly in IO, so as to allow testing with an alternative monad
runSudokuMain :: MonadConsole m => m ()
runSudokuMain = do
  choice <- prompt
  shouldContinue <- process choice
  if shouldContinue then runSudokuMain else pure ()
  pure ()

prompt :: MonadConsole m => m Choice
prompt = do
    consoleWrite $ renderGrid emptyGrid
    consoleWrite menuOptions
    readMenuChoice

process :: Monad m => Choice -> m Bool
process choice = pure $ choice /= Quit 

instance MonadConsole IO where
  consoleWrite = putStrLn
  consoleReadLine = getLine 

data Choice = Quit | NewGrid | Redisplay
  deriving (Eq, Show)


menuOptions :: String
menuOptions = "Choose: \n\
              \n: new grid\n\
              \q: quit"

readMenuChoice :: MonadConsole m => m Choice
readMenuChoice = do
  input <- consoleReadLine
  pure $ case input of
    "q" -> Quit
    "n" -> NewGrid
    _ -> Redisplay
