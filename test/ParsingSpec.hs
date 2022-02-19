module ParsingSpec where

import Test.Hspec
import Sudoku.Grid
import Sudoku.Parsing

spec :: Spec
spec = 
    describe "grid parsing - parked" $ do
        it "rejects grids that are too short" $
            parseGrid "" `shouldBe` Nothing

        it "rejects grid with missing row" $
            parseGrid ".........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n"
                `shouldBe` Nothing 

        it "rejects grid with extra row" $
            parseGrid ".........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n"
                `shouldBe` Nothing

        it "rejects grid with extra char" $
            parseGrid ".........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \..........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n"
                `shouldBe` Nothing 

        it "accepts an empty grid" $
            parseGrid ".........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n\
                      \.........\n"
                `shouldBe` Just emptyGrid 
