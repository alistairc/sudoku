{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ConsoleUiSpec where

import ConsoleUi.ConsoleApp
import Control.Monad.State
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec

spec :: Spec
spec =
  describe "on startup" $
    it "should display an empty grid" $
      let consoleOutput = runTestConsoleApp runSudoku
       in lastOutput consoleOutput `shouldBe` renderGrid emptyGrid


lastOutput :: [String] -> String
lastOutput [] = "" -- to avoid the partial
lastOutput list = head list

-- custom monad for side-effect free testing
newtype TestConsoleApp a = TestConsoleApp (State [String] a)
  deriving (Functor, Applicative, Monad, MonadState [String])

-- run function for our custom monad.
runTestConsoleApp :: TestConsoleApp a -> [String]
runTestConsoleApp (TestConsoleApp stdOutState) = execState stdOutState []

instance MonadConsole TestConsoleApp where
  consoleWrite text = modify (text :) -- not a smiley! :) add test to head of list
