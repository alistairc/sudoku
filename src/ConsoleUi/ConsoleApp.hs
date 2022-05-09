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
run StartMove = do
  consoleWrite "Row?"
  char <- consoleReadChar
  let row = parseRow char
  pure $ maybe StartMove PromptColumn row
run (PromptColumn row) = do
  consoleWrite "Column?"
  char <- consoleReadChar
  let col = parseColumn char
  pure $ maybe (PromptColumn row) (PromptDigit row) col
run (PromptDigit row col) = do
  consoleWrite "Digit?"
  char <- consoleReadChar
  let digit = parseDigit char
  pure $
    maybe
      (PromptDigit row col)
      (const MainMenu)
      digit

parseRow :: Char -> Maybe Row
parseRow char = numToRow <$> (readMaybe [char] :: Maybe Int)
  where
    numToRow i = toEnum (i - 1)

parseColumn :: Char -> Maybe Column
parseColumn char = numToCol <$> (readMaybe [char] :: Maybe Int)
  where
    numToCol i = toEnum (i - 1)

parseDigit :: Char -> Maybe Digit
parseDigit char = numToCol <$> (readMaybe [char] :: Maybe Int)
  where
    numToCol i = toEnum (i - 1)

menuOptions :: String
menuOptions =
  "Choose: \n\
  \n: new grid\n\
  \q: quit"

parseMenuChoice :: Char -> Choice
parseMenuChoice input
  | input == 'q' = Quit
  | input == 'n' = NewGrid
  | input == 'm' = StartMove
  | otherwise = MainMenu
