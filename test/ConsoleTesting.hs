module ConsoleTesting where

import ConsoleUi.ConsoleIO
import Control.Monad.State

data ConsoleState = ConsoleState
  { writtenOutput :: [String],
    keyPresses :: [Char]
  }
  deriving (Eq, Show)

-- custom monad for side-effect free testing
newtype TestConsoleApp a = TestConsoleApp (State ConsoleState a)
  deriving (Functor, Applicative, Monad, MonadState ConsoleState)

getConsoleLines :: TestConsoleApp [String]
getConsoleLines = gets (reverse . writtenOutput)

-- run function for our custom monad.
runTestConsoleApp :: [Char] -> TestConsoleApp a -> a
runTestConsoleApp consoleInput (TestConsoleApp stdOutState) =
  evalState stdOutState initialState
  where
    initialState = ConsoleState {writtenOutput = [], keyPresses = consoleInput}

instance MonadConsole TestConsoleApp where
  consoleWrite text = modify recordOutput
    where
      recordOutput :: ConsoleState -> ConsoleState
      recordOutput s = s {writtenOutput = text : writtenOutput s}

  consoleReadChar = do
    chars <- gets keyPresses
    let (current, rest) = split chars
    modify (\s -> s {keyPresses = rest})
    pure current
    where
      split [] = (' ', [])
      split (h : t) = (h, t)
