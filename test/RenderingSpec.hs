module RenderingSpec where 

import Test.Hspec
import Sudoku.Grid
import Sudoku.Rendering

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

