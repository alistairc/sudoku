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
            `shouldBe` [ "first line one",
                         "second line two"
                       ]
    it "should use blanks if it runs out of input lines" $
      let inputLines = []
          line = runTestConsoleApp inputLines consoleReadLine
       in line `shouldBe` ""

  describe "on startup" $ do
    it "should display an empty grid, then a menu" $
      let consoleOutput = runTestConsoleApp ["q"] $ do
            runSudokuMain
            getConsoleLines
       in consoleOutput
            `shouldBe` [ renderGrid emptyGrid,
                         menuOptions
                       ]
    it "should loop until quit" $
      let consoleOutput = runTestConsoleApp ["x","x","q"] $ do
            runSudokuMain
            getConsoleLines
       in
         length consoleOutput `shouldBe` 6

    context "menu selections" $ do
      it "q -> should quit" $
        let consoleInputLines = ["q"]
            menuChoice = runTestConsoleApp consoleInputLines prompt
         in menuChoice `shouldBe` Quit

      it "n -> new grid" $
        let consoleInputLines = ["n"]
            menuChoice = runTestConsoleApp consoleInputLines prompt
         in menuChoice `shouldBe` NewGrid
         
      it "any other key -> redisplay menu" $
        let consoleInputLines = ["x"]
            menuChoice = runTestConsoleApp consoleInputLines prompt
         in menuChoice `shouldBe` Redisplay

data ConsoleState = ConsoleState
  { writtenOutput :: [String],
    remainingInput :: [String]
  }
  deriving (Eq, Show)

-- custom monad for side-effect free testing
newtype TestConsoleApp a = TestConsoleApp (State ConsoleState a)
  deriving (Functor, Applicative, Monad, MonadState ConsoleState)

getConsoleLines :: TestConsoleApp [String]
getConsoleLines = gets (reverse . writtenOutput)

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
      split (h : t) = (h, t)
