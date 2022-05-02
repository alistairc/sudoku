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


      -- next up, make a move.  will be a complex iteraction.  not sure how best to write the test
      -- will need to track state of grid too
      -- maybe check the console interaction against a script?
      --i.e.
      -- < m
      -- > row?
      -- < 4
      -- > column?
      -- < 9
      -- > digit?
      -- < 2
      -- 
      -- validation makes this quite complex too
      -- the validation itself is pure though
      -- menu 'm' should make a move
      -- making a move:
      --    should prompt for row
      --      row, if valid <- prompt for column
      --      row, if invalid





