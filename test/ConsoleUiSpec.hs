module ConsoleUiSpec where

import ConsoleTesting
import ConsoleUi.ConsoleApp
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
      it "start of move -> prompt for row" $
        let consoleOutput = runTestConsoleApp ['3'] $ do
              run StartMove
              getConsoleLines
         in consoleOutput `shouldBe` ["Row?"]
      it "valid row 3" $
        let nextAction = runTestConsoleApp ['3'] $ run StartMove
         in nextAction `shouldBe` PromptColumn R3
      it "valid row 9" $
        let nextAction = runTestConsoleApp ['9'] $ run StartMove
         in nextAction `shouldBe` PromptColumn R9
      it "invalid row -> should retry" $
        let nextAction = runTestConsoleApp ['x'] $ run StartMove
         in nextAction `shouldBe` StartMove

