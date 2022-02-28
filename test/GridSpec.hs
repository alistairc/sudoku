module GridSpec where

import Data.Function ((&))
import Data.Maybe (isJust)
import Sudoku.Grid
import Sudoku.Rendering
import Test.Hspec
import TestData

spec :: Spec
spec =
  withSampleGrid
    ( \sampleGrid -> do
        describe "grid list conversions" $ do
          it "gridToList, empty grid" $
            gridToList emptyGrid `shouldBe` replicate 81 Nothing
          it "gridToList, full grid" $
            let list = gridToList sampleGrid
             in do
                  head list `shouldBe` Just D1
                  list !! 10 `shouldBe` Just D3
                  list !! 80 `shouldBe` Just D8
          it "listToGrid, not enough squares -> Nothing" $
            let squares = replicate 80 (Just D1)
             in listToGrid squares `shouldBe` Nothing
          it "listToGrid, 81 squares, sets squares" $
            let squares = [Just D9] ++ replicate 79 (Just D1) ++ [Nothing]
                maybeGrid = listToGrid squares
             in case maybeGrid of
                  Just grid -> do
                    getSquare (C1, R1) grid `shouldBe` Just D9
                    getSquare (C2, R2) grid `shouldBe` Just D1
                    getSquare (C9, R9) grid `shouldBe` Nothing
                  Nothing ->
                    maybeGrid `shouldNotBe` Nothing

        describe "select single square" $
          it "getSquare" $ do
            getSquare (C1, R1) sampleGrid `shouldBe` Just D1
            getSquare (C2, R2) sampleGrid `shouldBe` Just D3
            getSquare (C1, R1) emptyGrid `shouldBe` Nothing

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

        describe "selecting a row" $
          it "selects the digits" $ do
            selectRow R1 sampleGrid `shouldBe` DigitSet [Just D1, Just D2, Just D3, Just D4, Just D5, Just D6, Just D7, Just D8, Just D9]
            selectRow R9 sampleGrid `shouldBe` DigitSet [Just D9, Just D1, Just D2, Just D3, Just D4, Just D5, Just D6, Just D7, Just D8]
    )
