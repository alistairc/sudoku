module ConsoleUiSpec where

import ConsoleTesting
import ConsoleUi.ConsoleApp
import Control.Monad (forM_)
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec


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
      it "should display an empty grid, then a menu" $
        let consoleOutput = runTestConsoleApp ['q'] $ do
              run MainMenu
              getConsoleLines
         in consoleOutput
              `shouldBe` [ renderGrid emptyGrid,
                           menuOptions
                         ]
      it "q -> should quit" $
        let keyPresses = ['q']
            menuChoice = runTestConsoleApp keyPresses $ run MainMenu
         in menuChoice `shouldBe` Quit

      it "n -> new grid" $
        let keyPresses = ['n']
            menuChoice = runTestConsoleApp keyPresses $ run MainMenu
         in menuChoice `shouldBe` NewGrid

      it "m -> make a move" $ do
        let menuChoice = runTestConsoleApp ['m'] $ run MainMenu
        menuChoice `shouldBe` StartMove

      it "any other key -> redisplay menu" $
        let keyPresses = ['x']
            menuChoice = runTestConsoleApp keyPresses $ run MainMenu
         in menuChoice `shouldBe` MainMenu

    context "Making a Move" $ do
      context "StartMove" $ do
        let currentAction = StartMove
        it "should prompt for row" $
            choicePrompt currentAction `shouldBe` ["Row?"]
        cases (zip [R1 .. R9] ['1' .. '9']) $ \(row, char) -> do
          it "valid row -> should prompt for column" $
            let nextAction = runTestConsoleApp [char] $ run currentAction
             in nextAction `shouldBe` PromptColumn row
        it "invalid row -> should retry" $
          let nextAction = runTestConsoleApp ['x'] $ run currentAction
           in nextAction `shouldBe` currentAction

      context "PromptColumn" $ do
        cases [R1 .. R9] $ \row -> do
          let currentAction = PromptColumn row
          it "should prompt for column" $
            choicePrompt currentAction `shouldBe` ["Column?"]
          it "invalid column -> should retry" $
            let nextAction = runTestConsoleApp ['x'] $ run currentAction
             in nextAction `shouldBe` currentAction
          cases (zip [C1 .. C9] ['1' .. '9']) $ \(col, char) -> do
            it "valid column -> should prompt for digit" $
              let nextAction = runTestConsoleApp [char] $ run currentAction
               in nextAction `shouldBe` PromptDigit row col

      context "Prompt Digit" $ do
        cases
          [ (R1, C2),
            (R9, C8)
          ]
          $ \(row, col) -> do
            let currentAction = PromptDigit row col
            it "should prompt for digit" $
              choicePrompt currentAction `shouldBe` ["Digit?"]
            it "invalid digit -> should retry" $
              let nextAction = runTestConsoleApp ['x'] $ run currentAction
               in nextAction `shouldBe` currentAction
            cases (zip [D1 .. D9] ['1' .. '9']) $ \(digit, char) -> do
              it "valid digit -> should return to main menu" $
                let nextAction = runTestConsoleApp [char] $ run currentAction
                 in nextAction `shouldBe` MainMenu

choicePrompt :: Choice -> [String]
choicePrompt action = runTestConsoleApp [] $ do
  run action
  getConsoleLines

cases :: Show a => [a] -> (a -> SpecWith b) -> SpecWith b
cases caseList specs = forM_ caseList runInContext
  where
    runInContext x = context (show x) $ specs x
