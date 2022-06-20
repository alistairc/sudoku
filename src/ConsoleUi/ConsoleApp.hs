module ConsoleUi.ConsoleApp
  ( Choice (..),
    menuOptions,
    runSudokuMain,
    run,
  )
where

import ConsoleUi.ConsoleIO
import ConsoleUi.GridState
import Data.Function ((&))
import Sudoku.Grid
import Sudoku.Rendering
import Text.Read

data Choice = MainMenu | Quit | NewGrid | StartMove | PromptColumn Row | PromptDigit Row Column
  deriving (Eq, Show)

-- called by main itself but not directly in IO, so as to allow testing with an alternative monad
runSudokuMain :: (MonadConsole m, MonadGrid m) => m ()
runSudokuMain = do
  setGrid emptyGrid
  go MainMenu
  where
    go choice = do
      next <- run choice
      if next == Quit then pure () else go next

run :: (MonadConsole m, MonadGrid m) => Choice -> m Choice
run choice =
  case choice of
    Quit -> pure Quit
    MainMenu -> mainMenu
    NewGrid -> newGrid
    StartMove -> startMove
    PromptColumn row -> chooseColumn row
    PromptDigit row col -> chooseDigit row col

mainMenu :: (MonadConsole m, MonadGrid m) => m Choice
mainMenu = do
  getGrid >>= consoleWrite . renderGrid
  consoleWrite menuOptions
  char <- consoleReadChar
  let next = parseMenuChoice char
  pure next

newGrid :: (MonadConsole m, MonadGrid m) => m Choice
newGrid = do 
  setGrid emptyGrid
  pure MainMenu

startMove :: (MonadConsole m, MonadGrid m) => m Choice
startMove = do
  consoleWrite "Row?"
  char <- consoleReadChar
  let row = parseNumeric char
  let next = maybe StartMove PromptColumn row
  pure next

chooseColumn :: (MonadConsole m, MonadGrid m) => Row -> m Choice
chooseColumn row = do
  consoleWrite "Column?"
  char <- consoleReadChar
  let col = parseNumeric char
  let next = maybe (PromptColumn row) (PromptDigit row) col
  pure next

chooseDigit :: (MonadConsole m, MonadGrid m) => Row -> Column -> m Choice
chooseDigit row col = do
  consoleWrite "Digit?"
  char <- consoleReadChar
  let digit = parseNumeric char
  maybe 
    (pure $ PromptDigit row col)
    (\d -> do 
      updateGrid $ moveAt (col, row) d
      pure MainMenu
    )
    digit

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
