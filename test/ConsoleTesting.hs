module ConsoleTesting where

import ConsoleUi.ConsoleIO
import Control.Monad.State
import Sudoku.Grid (emptyGrid, Grid)

data ConsoleState = ConsoleState
  { writtenOutput :: [String],
    keyPresses :: [Char]
  }
  deriving (Eq, Show)

data TestState = TestState {
  consoleState :: ConsoleState,
  currentGrid :: Grid
}

-- custom monad for side-effect free testing
newtype TestConsoleApp a = TestConsoleApp (State TestState a)
  deriving (Functor, Applicative, Monad, MonadState TestState)

getConsoleLines :: TestConsoleApp [String]
getConsoleLines = gets (reverse . writtenOutput . consoleState)

-- run function for our custom monad.
runTestConsoleApp :: [Char] -> TestConsoleApp a -> a
runTestConsoleApp consoleInput (TestConsoleApp stdOutState) =
  evalState stdOutState initialState
  where
    initialState = TestState { 
      consoleState = ConsoleState {writtenOutput = [], keyPresses = consoleInput},
      currentGrid = emptyGrid
    }

instance MonadConsole TestConsoleApp where
  consoleWrite text = modify recordOutput
    where
      recordOutput :: TestState -> TestState
      recordOutput s = 
        let
           existingOutput = (writtenOutput . consoleState) s
           newOutput = text : existingOutput
        in 
          s { consoleState = (consoleState s) { writtenOutput = newOutput}}

  consoleReadChar = do
    chars <- gets (keyPresses . consoleState)
    let (current, rest) = split chars
    modify (\s -> s { consoleState = (consoleState s) {keyPresses = rest}})
    pure current
    where
      split [] = (' ', [])
      split (h : t) = (h, t)

instance MonadGrid TestConsoleApp where
  getGrid = pure emptyGrid
  setGrid _ = pure ()