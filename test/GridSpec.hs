module GridSpec where

import Data.Function ((&))
import Data.Maybe (isJust)
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec
import TestData

spec :: Spec
spec = do
  describe "grid list conversions" $ do
    it "gridToList, empty grid" $
      gridToList emptyGrid `shouldBe` replicate 81 Nothing
    it "gridToList, full grid" $
      let list = gridToList sampleGrid
       in do
            head list `shouldBe` Just D1
            list !! 10 `shouldBe` Just D3
            list !! 80 `shouldBe` Just D8

    context "listToGrid" $ do
      it "empty list -> emptyGrid" $
        listToGrid [] `shouldBe` emptyGrid

      it "listToGrid, Not enough cells, sets first cells" $
        let cells = [Just D1, Just D2, Just D3]
            grid = listToGrid cells
         in do
              getCell (C1, R1) grid `shouldBe` Just D1
              getCell (C2, R1) grid `shouldBe` Just D2
              getCell (C3, R1) grid `shouldBe` Just D3
              getCell (C4, R1) grid `shouldBe` Nothing
              getCell (C1, R2) grid `shouldBe` Nothing
              getCell (C9, R9) grid `shouldBe` Nothing

  describe "select single cell" $
    it "getCell" $ do
      getCell (C1, R1) sampleGrid `shouldBe` Just D1
      getCell (C2, R2) sampleGrid `shouldBe` Just D3
      getCell (C1, R1) emptyGrid `shouldBe` Nothing

  describe "making a move" $ do
    it "allows valid moves" $
      (emptyGrid & moveAt (C1, R1) D1 & renderGrid)
        `shouldBe` "1........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n"

    it "allows valid moves x" $
      (emptyGrid & moveAt (C9, R1) D2 & renderGrid)
        `shouldBe` "........2\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n"

    it "allows valid moves y" $
      (emptyGrid & moveAt (C1, R9) D3 & renderGrid)
        `shouldBe` ".........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \.........\n\
                   \3........\n"

    it "moves compose" $
      let newGrid = emptyGrid & moveAt (C1, R1) D1 & moveAt (C2, R2) D2
       in renderGrid newGrid
            `shouldBe` "1........\n\
                       \.2.......\n\
                       \.........\n\
                       \.........\n\
                       \.........\n\
                       \.........\n\
                       \.........\n\
                       \.........\n\
                       \.........\n"


  describe "digitSets" $ do
    context "digitSetFromList" $ do
      it "emptyList -> 9 Nothings" $
        digitSetFromList [] `shouldBe` emptyDigitSet 
      it "partial list -> 9 digits" $
        digitSetFromList [Just D1, Just D2, Just D3] `shouldBe` DigitSet (Just D1) (Just D2) (Just D3) Nothing Nothing Nothing Nothing Nothing Nothing
    context "digitSetToList" $
      it "returns the digits" $
        digitSetToList (DigitSet (Just D1) (Just D2) (Just D3) (Just D4) (Just D5) (Just D6) (Just D7) (Just D8) (Just D9)) 
        `shouldBe` 
          [Just D1, Just D2, Just D3, Just D4, Just D5, Just D6, Just D7, Just D8, Just D9]

  describe "selecting a row" $
    it "selects the digits" $ do
      selectRow R1 sampleGrid `shouldBe` DigitSet (Just D1) (Just D2) (Just D3) (Just D4) (Just D5) (Just D6) (Just D7) (Just D8) (Just D9)
      selectRow R9 sampleGrid `shouldBe` DigitSet (Just D9) (Just D1) (Just D2) (Just D3) (Just D4) (Just D5) (Just D6) (Just D7) (Just D8)
