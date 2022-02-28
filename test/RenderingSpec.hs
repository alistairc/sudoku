module RenderingSpec where 

import Test.Hspec
import Sudoku.Grid
import Sudoku.Rendering
import Data.Function ((&))
import TestData

spec :: Spec
spec = withSampleGrid (\sampleGrid -> do
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
    )