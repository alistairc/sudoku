module ConsoleUi.ConsoleApp where

import ConsoleUi.ConsoleIO
import Sudoku.Grid
import Sudoku.Rendering


-- called by main itself but not directly in IO, so as to allow testing with an alternative monad
runSudokuMain :: MonadConsole m => m ()
runSudokuMain = do
  choice <- prompt
  let shouldContinue = process choice  --TODO : this will need to become unpure at some point
  if shouldContinue then runSudokuMain else pure ()

prompt :: MonadConsole m => m Choice
prompt = do
    consoleWrite $ renderGrid emptyGrid
    consoleWrite menuOptions
    parseMenuChoice <$> consoleReadChar

process :: Choice -> Bool
process choice = choice /= Quit

data Choice = Quit | NewGrid | Redisplay
  deriving (Eq, Show)

menuOptions :: String
menuOptions = "Choose: \n\
              \n: new grid\n\
              \q: quit"

parseMenuChoice :: Char -> Choice
parseMenuChoice input
  | input == 'q' = Quit
  | input == 'n' = NewGrid
  | otherwise = Redisplay
