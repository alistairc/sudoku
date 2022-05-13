module ConsoleUi.ConsoleApp where

import ConsoleUi.ConsoleIO
import Data.Function ((&))
import Sudoku.Grid
import Sudoku.Rendering
import Text.Read

data Choice = MainMenu | Quit | NewGrid | StartMove | PromptColumn Row | PromptDigit Row Column
  deriving (Eq, Show)

-- called by main itself but not directly in IO, so as to allow testing with an alternative monad
runSudokuMain :: MonadConsole m => m ()
runSudokuMain = do
  go MainMenu emptyGrid
  where
    go choice grid = do
      (next, nextGrid) <- run choice grid
      if next == Quit then pure () else go next nextGrid

run :: MonadConsole m => Choice -> Grid -> m (Choice, Grid)
run choice grid =
  case choice of
    Quit -> pure (Quit, grid)
    MainMenu -> do
      consoleWrite $ renderGrid grid
      consoleWrite menuOptions
      char <- consoleReadChar
      let next = parseMenuChoice char
      pure (next, grid)
    NewGrid ->
      pure (MainMenu, grid)
    StartMove -> do
      consoleWrite "Row?"
      char <- consoleReadChar
      let row = parseNumeric char
      let next = maybe StartMove PromptColumn row
      pure (next, grid)
    (PromptColumn row) -> do
      consoleWrite "Column?"
      char <- consoleReadChar
      let col = parseNumeric char
      let next = maybe (PromptColumn row) (PromptDigit row) col
      pure (next, grid)
    (PromptDigit row col) -> do
      consoleWrite "Digit?"
      char <- consoleReadChar
      let digit = (parseNumeric char :: Maybe Digit)
      let result =
            maybe
              (PromptDigit row col, grid)
              (\d -> (MainMenu, grid & moveAt (col, row) d))
              digit
      pure result

parseNumeric :: Enum a => Char -> Maybe a
parseNumeric char = convert <$> (readMaybe [char] :: Maybe Int)
  where
    convert i = toEnum (i - 1)

menuOptions :: String
menuOptions =
  "Choose: \n\
  \n: new grid\n\
  \m: move\n\
  \q: quit"

parseMenuChoice :: Char -> Choice
parseMenuChoice input
  | input == 'q' = Quit
  | input == 'n' = NewGrid
  | input == 'm' = StartMove
  | otherwise = MainMenu
