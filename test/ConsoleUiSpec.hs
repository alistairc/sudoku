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
       in
         length consoleOutput `shouldSatisfy`  (>2)

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
         
      it "any other key -> redisplay menu" $
        let keyPresses = ['x']
            menuChoice = runTestConsoleApp keyPresses $ run MainMenu
         in menuChoice `shouldBe` MainMenu

