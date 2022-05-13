module ConsoleUi.ConsoleApp
  ( Choice (..),
    menuOptions,
    runSudokuMain,
    run,
  )
where

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
    MainMenu -> doMainMenu grid
    NewGrid -> pure (MainMenu, grid)
    StartMove -> startMove grid
    PromptColumn row -> chooseColumn grid row
    PromptDigit row col -> chooseDigit grid row col

doMainMenu :: MonadConsole m => Grid -> m (Choice, Grid)
doMainMenu grid = do
  consoleWrite $ renderGrid grid
  consoleWrite menuOptions
  char <- consoleReadChar
  let next = parseMenuChoice char
  pure (next, grid)

startMove :: MonadConsole m => Grid -> m (Choice, Grid)
startMove grid = do
  consoleWrite "Row?"
  char <- consoleReadChar
  let row = parseNumeric char
  let next = maybe StartMove PromptColumn row
  pure (next, grid)

chooseColumn :: MonadConsole m => Grid -> Row -> m (Choice, Grid)
chooseColumn grid row = do
  consoleWrite "Column?"
  char <- consoleReadChar
  let col = parseNumeric char
  let next = maybe (PromptColumn row) (PromptDigit row) col
  pure (next, grid)

chooseDigit :: MonadConsole m => Grid -> Row -> Column -> m (Choice, Grid)
chooseDigit grid row col = do
  consoleWrite "Digit?"
  char <- consoleReadChar
  let digit = parseNumeric char :: Maybe Digit
  let result =
        maybe
          (PromptDigit row col, grid)
          (\d -> (MainMenu, grid & moveAt (col, row) d))
          digit
  pure result

promptNumeric :: (MonadConsole m, Enum b) => String -> m (Maybe b)
promptNumeric prompt = do
  consoleWrite prompt
  parseNumeric <$> consoleReadChar

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
