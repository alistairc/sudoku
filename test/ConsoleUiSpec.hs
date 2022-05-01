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
            firstChar <- consoleReadChar
            consoleWrite $ "first " ++ [firstChar]

            secondChar <- consoleReadChar
            consoleWrite $ "second " ++ [secondChar]

            getConsoleLines
          inputChars = ['1', '2', '3']  -- last one will be ignored
          recordedOutput = runTestConsoleApp inputChars sampleProgram
       in recordedOutput
            `shouldBe` [ "first 1",
                         "second 2"
                       ]
    it "should use spaces if it runs out of input chars" $
      let inputChars = []
          readChar = runTestConsoleApp inputChars consoleReadChar
       in readChar `shouldBe` ' '

  describe "on startup" $ do
    it "should display an empty grid, then a menu" $
      let consoleOutput = runTestConsoleApp ['q'] $ do
            runSudokuMain
            getConsoleLines
       in consoleOutput
            `shouldBe` [ renderGrid emptyGrid,
                         menuOptions
                       ]
    it "should loop until quit" $
      let consoleOutput = runTestConsoleApp "xxq" $ do
            runSudokuMain
            getConsoleLines
       in
         length consoleOutput `shouldBe` 6

    context "menu selections" $ do
      it "q -> should quit" $
        let keyPresses = ['q']
            menuChoice = runTestConsoleApp keyPresses prompt
         in menuChoice `shouldBe` Quit

      it "n -> new grid" $
        let keyPresses = ['n']
            menuChoice = runTestConsoleApp keyPresses prompt
         in menuChoice `shouldBe` NewGrid
         
      it "any other key -> redisplay menu" $
        let keyPresses = ['x']
            menuChoice = runTestConsoleApp keyPresses prompt
         in menuChoice `shouldBe` Redisplay

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
