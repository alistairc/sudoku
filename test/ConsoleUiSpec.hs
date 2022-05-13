module ConsoleUiSpec where

import ConsoleTesting
import ConsoleUi.ConsoleApp
import Control.Monad (forM_)
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec
import TestData (sampleGrid)

spec :: Spec
spec = do
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
       in length consoleOutput `shouldSatisfy` (> 2)

    context "Main Menu" $ do
      it "should display a grid, then a menu" $
        let consoleOutput = runTestConsoleApp ['q'] $ do
              run MainMenu sampleGrid
              getConsoleLines
         in consoleOutput
              `shouldBe` [ renderGrid sampleGrid,
                           menuOptions
                         ]
      it "q -> should quit" $
        nextActionShouldBe MainMenu 'q' Quit

      it "n -> new grid" $
        nextActionShouldBe MainMenu 'n' NewGrid

      it "m -> make a move" $ do
        nextActionShouldBe MainMenu 'm' StartMove

      it "any other key -> redisplay menu" $
        nextActionShouldBe MainMenu 'x' MainMenu

    context "Making a Move" $ do
      context "StartMove" $ do
        let currentAction = StartMove
        it "should prompt for row" $
          promptForChoice currentAction `shouldBe` ["Row?"]
        it "invalid row -> should retry" $
          invalidInputShouldRetry currentAction 'x'
        cases
          [ ('1', R1),
            ('9', R9)
          ]
          $ \(char, row) -> do
            it "valid row -> should prompt for column" $
              nextActionShouldBe currentAction char $ PromptColumn row

      context "PromptColumn" $ do
        cases [R1, R9] $ \row -> do
          let currentAction = PromptColumn row
          it "should prompt for column" $
            promptForChoice currentAction `shouldBe` ["Column?"]
          it "invalid column -> should retry" $
            invalidInputShouldRetry currentAction 'x'
          cases
            [ ('1', C1),
              ('9', C9)
            ]
            $ \(char, col) -> do
              it "valid column -> should prompt for digit" $
                nextActionShouldBe currentAction char $ PromptDigit row col

      context "Prompt Digit" $ do
        cases
          [ (R1, C2),
            (R9, C8)
          ]
          $ \(row, col) -> do
            let currentAction = PromptDigit row col
            it "should prompt for digit" $
              promptForChoice currentAction `shouldBe` ["Digit?"]
            it "invalid digit -> should retry" $
              invalidInputShouldRetry currentAction 'x'
            cases
              [ ('1', D1),
                ('9', D9)
              ]
              $ \(char, digit) -> do
               it "valid digit -> should return to main menu" $
                 nextActionShouldBe currentAction char MainMenu
               it "should update the grid" $
                let 
                  initialGrid = emptyGrid
                  (_, newGrid) = runTestConsoleApp [char] $ run currentAction initialGrid
                in
                  getCell (col,row) newGrid `shouldBe` Just digit



promptForChoice :: Choice -> [String]
promptForChoice action = runTestConsoleApp [] $ do
  run action emptyGrid
  getConsoleLines

invalidInputShouldRetry :: Choice -> Char -> Expectation
invalidInputShouldRetry currentChoice invalidInput =
  let (nextAction,_) = runTestConsoleApp [invalidInput] $ run currentChoice emptyGrid
   in nextAction `shouldBe` currentChoice

nextActionShouldBe :: Choice -> Char -> Choice -> Expectation
nextActionShouldBe currentAction char expectedNext =
  let (nextAction,_) = runTestConsoleApp [char] $ run currentAction emptyGrid
   in nextAction `shouldBe` expectedNext

cases :: Show a => [a] -> (a -> SpecWith b) -> SpecWith b
cases caseList specs = forM_ caseList runInContext
  where
    runInContext x = context (show x) $ specs x
