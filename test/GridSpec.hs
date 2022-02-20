module GridSpec where

import Test.Hspec
import Sudoku.Grid
import Sudoku.Rendering
import Data.Function ((&))

sampleGrid = Grid [ 
        Just D1,Just D2,Just D3,Just D4,Just D5,Just D6,Just D7,Just D8,Just D9,
        Just D2,Just D3,Just D4,Just D5,Just D6,Just D7,Just D8,Just D9,Just D1,
        Just D3,Just D4,Just D5,Just D6,Just D7,Just D8,Just D9,Just D1,Just D2,
        Just D4,Just D5,Just D6,Just D7,Just D8,Just D9,Just D1,Just D2,Just D3,
        Just D5,Just D6,Just D7,Just D8,Just D9,Just D1,Just D2,Just D3,Just D4,
        Just D6,Just D7,Just D8,Just D9,Just D1,Just D2,Just D3,Just D4,Just D5,
        Just D7,Just D8,Just D9,Just D1,Just D2,Just D3,Just D4,Just D5,Just D6,
        Just D8,Just D9,Just D1,Just D2,Just D3,Just D4,Just D5,Just D6,Just D7,
        Just D9,Just D1,Just D2,Just D3,Just D4,Just D5,Just D6,Just D7,Just D8
    ]

spec :: Spec
spec = do
    describe "making a move" $ do
        it "allows valid moves" $
            (emptyGrid & moveAt (C1, R1) D1 & renderGrid) `shouldBe`
                "1........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n"

        it "allows valid moves x" $
            (emptyGrid & moveAt (C9, R1) D2 & renderGrid) `shouldBe`
                "........2\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n"

        it "allows valid moves y" $
            (emptyGrid & moveAt (C1, R9) D3 & renderGrid) `shouldBe`
                ".........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \3........\n"

        it "moves compose" $
            let newGrid = emptyGrid & moveAt (C1,R1) D1 & moveAt (C2,R2) D2
            in
                renderGrid newGrid `shouldBe`
                    "1........\n\
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
            selectRow R1 sampleGrid `shouldBe` DigitSet [Just D1,Just D2,Just D3,Just D4,Just D5,Just D6,Just D7,Just D8,Just D9]
            selectRow R9 sampleGrid `shouldBe` DigitSet [Just D9,Just D1,Just D2,Just D3,Just D4,Just D5,Just D6,Just D7,Just D8]

