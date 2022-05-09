module ConsoleUiSpec where

import ConsoleTesting
import ConsoleUi.ConsoleApp
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec
import Control.Monad (forM_)

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
        it "should prompt for row" $
          let consoleOutput = runTestConsoleApp ['3'] $ do
                run StartMove
                getConsoleLines
          in consoleOutput `shouldBe` ["Row?"]
        cases (zip [R1 .. R9] ['1' .. '9']) $ \(row,char) -> do
          it "valid row" $
            let nextAction = runTestConsoleApp [char] $ run StartMove
            in nextAction `shouldBe` PromptColumn row
        it "invalid row -> should retry" $
          let nextAction = runTestConsoleApp ['x'] $ run StartMove
          in nextAction `shouldBe` StartMove

      context "PromptColumn" $ do
        cases [R1 .. R9] $ \row -> do
            it "should prompt for column" $
              let consoleOutput = runTestConsoleApp ['3'] $ do
                    run $ PromptColumn row
                    getConsoleLines
              in consoleOutput `shouldBe` ["Column?"]
            it "invalid column -> should retry" $
              let nextAction = runTestConsoleApp ['x'] $ run $ PromptColumn row
              in nextAction `shouldBe` PromptColumn row
            cases (zip [C1 .. C9] ['1' .. '9']) $ \(col,char) -> do
              it "valid column -> should prompt for digit" $
                let nextAction = runTestConsoleApp [char] $ run $ PromptColumn row
                in nextAction `shouldBe` PromptDigit row col

cases :: Show a => [a] -> (a -> SpecWith b) -> SpecWith b
cases caseList specs = forM_ caseList runInContext
  where runInContext x = context (show x) $ specs x

