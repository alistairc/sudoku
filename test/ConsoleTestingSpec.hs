module ConsoleTestingSpec where

import ConsoleUi.ConsoleIO
import ConsoleTesting
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
