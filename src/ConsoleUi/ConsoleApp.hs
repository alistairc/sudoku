module ConsoleUi.ConsoleApp where

import ConsoleUi.ConsoleIO
import Sudoku.Grid
import Sudoku.Rendering
import Control.Monad.State
import System.Console.Haskeline
import Data.Function ((&))


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
              \m: make a move \n\
              \q: quit"

parseMenuChoice :: Char -> Choice
parseMenuChoice input
  | input == 'q' = Quit
  | input == 'n' = NewGrid
  | otherwise = Redisplay




data MainMenuChoice = Restart | Move
data MenuCharResult = QuitMenu | BadChoice Char | Parsed MainMenuChoice

mainMenu :: (MonadConsole m, MonadState Grid m) => m ()
mainMenu = do
  grid <- get
  consoleWrite $ renderGrid grid
  consoleWrite menuOptions
  key <- consoleReadChar
  let choice = parseMain key
  case choice of
    QuitMenu -> pure ()
    BadChoice _ -> mainMenu
    Parsed choice -> doMenu choice

parseMain  :: Char -> MenuCharResult
parseMain char = case char of
  'q' -> QuitMenu
  'n' -> Parsed Restart
  'm' -> Parsed Move
  unknown -> BadChoice unknown

doMenu ::(MonadConsole m, MonadState Grid m) =>  MainMenuChoice -> m ()
doMenu Restart = do
  put emptyGrid
  mainMenu

doMenu Move = do
  consoleWrite "move stuff would happen now"
  mainMenu
 
experimentalMain :: IO ()
experimentalMain = runInputT defaultSettings (evalStateT mainMenu initialGrid)
  where initialGrid = emptyGrid & moveAt (C1,R2) D9


