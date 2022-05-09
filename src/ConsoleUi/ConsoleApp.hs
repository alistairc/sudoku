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
run choice =
  case choice of
    Quit -> pure Quit
    MainMenu -> do
      consoleWrite $ renderGrid emptyGrid
      consoleWrite menuOptions
      parseMenuChoice <$> consoleReadChar
    NewGrid ->
      pure MainMenu
    StartMove -> do
      consoleWrite "Row?"
      char <- consoleReadChar
      let row = parseNumeric char
      pure $ maybe StartMove PromptColumn row
    (PromptColumn row) -> do
      consoleWrite "Column?"
      char <- consoleReadChar
      let col = parseNumeric char
      pure $ maybe (PromptColumn row) (PromptDigit row) col
    (PromptDigit row col) -> do
      consoleWrite "Digit?"
      char <- consoleReadChar
      let digit = (parseNumeric char :: Maybe Digit)
      pure $
        maybe
          (PromptDigit row col)
          (const MainMenu)
          digit

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
