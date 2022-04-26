module ConsoleUiSpec where

import ConsoleUi.ConsoleApp
import Control.Monad.State
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec

spec :: Spec
spec = do
  describe "the console test monad" $ do
    it "should read input and record output" $
      let sampleProgram = do
            firstLine <- consoleReadLine
            consoleWrite $ "first " ++ firstLine

            secondLine <- consoleReadLine
            consoleWrite $ "second " ++ secondLine

            getConsoleLines
          inputLines = ["line one", "line two", "line 3 (ignored)"]
          recordedOutput = runTestConsoleApp inputLines sampleProgram
       in recordedOutput
            `shouldBe` [ "second line two",
                         "first line one"
                       ]
    it "should use blanks if it runs out of input lines" $
      let inputLines = []
          line = runTestConsoleApp inputLines consoleReadLine
       in line `shouldBe` ""

  describe "on startup" $
    it "should display an empty grid" $
      let consoleOutput = runTestConsoleApp [] $ do
            runSudoku
            getConsoleLines
       in lastOutput consoleOutput `shouldBe` renderGrid emptyGrid

getConsoleLines :: TestConsoleApp [String]
getConsoleLines = gets writtenOutput

lastOutput :: [String] -> String
lastOutput [] = "" -- to avoid the partial
lastOutput list = head list

data ConsoleState = ConsoleState
  { writtenOutput :: [String],
    remainingInput :: [String]
  }
  deriving (Eq, Show)

-- custom monad for side-effect free testing
newtype TestConsoleApp a = TestConsoleApp (State ConsoleState a)
  deriving (Functor, Applicative, Monad, MonadState ConsoleState)

-- run function for our custom monad.
runTestConsoleApp :: [String] -> TestConsoleApp a -> a
runTestConsoleApp consoleInput (TestConsoleApp stdOutState) =
  evalState stdOutState initialState
  where
    initialState = ConsoleState {writtenOutput = [], remainingInput = consoleInput}

instance MonadConsole TestConsoleApp where
  consoleWrite text = modify recordOutput
    where
      recordOutput :: ConsoleState -> ConsoleState
      recordOutput s = s {writtenOutput = text : writtenOutput s}

  consoleReadLine = do
    lines <- gets remainingInput
    let (current, rest) = split lines
    modify (\s -> s {remainingInput = rest})
    pure current
    where
      split [] = ("", [])
      split (h:t) = (h,t)
