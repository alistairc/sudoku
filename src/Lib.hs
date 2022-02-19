module Lib
--    ( someFunc
--    )
     where

import Data.List.Split
import Data.Function ((&))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
     deriving (Show, Eq)

newtype Grid = Grid [Maybe Digit]
     deriving (Show, Eq)

emptyGrid :: Grid
emptyGrid = Grid (replicate 81 Nothing)

renderSquare :: Maybe Digit -> Char
renderSquare Nothing = '.'
renderSquare (Just d) = case d of
    D1 -> '1'
    D2 -> '2'
    D3 -> '3'
    D4 -> '4'
    D5 -> '5'
    D6 -> '6'
    D7 -> '7'
    D8 -> '8'
    D9 -> '9'

renderGrid :: Grid -> String
renderGrid (Grid squares) =
     concatMap (\line -> renderLine line ++ ['\n']) (chunksOf 9 squares)

renderLine :: [Maybe Digit] -> String
renderLine = map renderSquare

parseGrid :: String -> Maybe Grid
parseGrid text = if length text /= 90 then Nothing else Just emptyGrid

moveAt :: (Int,Int) -> Digit -> Grid -> Grid
moveAt (x,y) digit initial = 
     let  
          (Grid squares) = initial
          index = (y - 1) * 9 + (x - 1) 
     in
          Grid $ take index squares ++ [Just digit] ++ drop (index + 1) squares

