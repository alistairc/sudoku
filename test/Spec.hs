import Test.Hspec
import Lib
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

main :: IO ()
main = hspec $ do
    describe "rendering" $ do
        it "empty grid is dots" $
            renderGrid emptyGrid `shouldBe`
                ".........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n\
                \.........\n"
        it "full grid is numbers" $
            renderGrid sampleGrid `shouldBe`
                "123456789\n\
                \234567891\n\
                \345678912\n\
                \456789123\n\
                \567891234\n\
                \678912345\n\
                \789123456\n\
                \891234567\n\
                \912345678\n"

    describe "making a move" $
        describe "valid moves" $ do
            it "allows valid moves" $
                (emptyGrid & moveAt (1, 1) D1 & renderGrid) `shouldBe`
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
                (emptyGrid & moveAt (9, 1) D2 & renderGrid) `shouldBe`
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
                (emptyGrid & moveAt (1, 9) D3 & renderGrid) `shouldBe`
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
                let newGrid = emptyGrid & moveAt (1,1) D1 & moveAt (2,2) D2
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



