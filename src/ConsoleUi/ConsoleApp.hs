module ConsoleUi.ConsoleApp where

import ConsoleUi.ConsoleIO
import Sudoku.Grid
import Sudoku.Rendering


-- called by main itself but not directly in IO, so as to allow testing with an alternative monad
runSudokuMain :: MonadConsole m => m ()
runSudokuMain = do
  go MainMenu
  where
    go choice = do
      next <- run choice
      if next == Quit then pure () else go next


run :: MonadConsole m => Choice -> m Choice
run Quit = pure Quit

run MainMenu = do
    consoleWrite $ renderGrid emptyGrid
    consoleWrite menuOptions
    parseMenuChoice <$> consoleReadChar

run NewGrid = pure MainMenu

run Redisplay = pure MainMenu

data Choice = MainMenu | Quit | NewGrid | Redisplay
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
