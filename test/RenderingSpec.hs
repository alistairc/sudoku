module RenderingSpec where 

import Test.Hspec
import Sudoku.Grid
import Sudoku.Rendering
import Data.Function ((&))

sampleGrid = listToGrid [ 
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

requireData :: (Show a, Eq a) => Maybe a -> (a -> Spec) -> Spec
requireData maybeData specs = 
    maybe (it "has required test data" (maybeData `shouldNotBe` Nothing)) specs maybeData

spec :: Spec
spec = requireData sampleGrid specsWithGrid

specsWithGrid :: Grid -> Spec
specsWithGrid grid = do
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
        renderGrid grid `shouldBe`
            "123456789\n\
            \234567891\n\
            \345678912\n\
            \456789123\n\
            \567891234\n\
            \678912345\n\
            \789123456\n\
            \891234567\n\
            \912345678\n"    