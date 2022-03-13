module TestData where

import Sudoku.Grid
import Test.Hspec

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
