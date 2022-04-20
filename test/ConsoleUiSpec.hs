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

-- this would be the main app function, called by main itself
-- but not directly in IO, so as to allow testing with an alternative monad
-- TODO: Move into app module
runSudoku :: (MonadConsole m) => m ()
runSudoku = consoleWrite $ renderGrid emptyGrid

lastOutput :: [String] -> String
lastOutput [] = "" -- to avoid the partial
lastOutput list = head list

-- custom monad for side-effect free testing
newtype TestConsoleApp a = TestConsoleApp (State [String] a)
  deriving (Functor, Applicative, Monad, MonadState [String])

-- run funciton for our custom monad.
runTestConsoleApp :: TestConsoleApp a -> [String]
runTestConsoleApp (TestConsoleApp stdOutState) = execState stdOutState []

-- typeclass as interface to abstract console IO
class Monad m => MonadConsole m where
  consoleWrite :: String -> m ()

instance MonadConsole TestConsoleApp where
  consoleWrite text = modify (text :) -- not a smiley! :) add test to head of list
