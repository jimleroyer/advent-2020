-- | This one could have been a lot simpler, but I went with trying out 
--   Haskell's data structure. 

{-# LANGUAGE OverloadedStrings #-}
module Day3
    ( part1, part2 ) where

import qualified Data.ByteString.Lazy.Char8 as BC (index, length, lines, readFile)

filename = "./data/day3-input.txt"
filenameExample = "./data/day3-input-example.txt"

data SledMove = SledMove { x :: Int,  y :: Int } deriving Show
data Dim = Dim { width :: Int, len :: Int } deriving Show

sledMove11 = SledMove 1 1
sledMove31 = SledMove 3 1
sledMove51 = SledMove 5 1
sledMove71 = SledMove 7 1
sledMove12 = SledMove 1 2

trace :: SledMove -> SledMove -> Int -> [SledMove]
trace init sledMove boundary = init : trace (move init sledMove boundary) sledMove boundary

move :: SledMove -> SledMove -> Int -> SledMove
move (SledMove x1 y1) (SledMove x2 y2) boundary
    | (x1 + x2) <  boundary = SledMove (x1 + x2) (y1 + y2)
    | (x1 + x2) >= boundary = SledMove remain (y1 + y2)
                    where remain = (x1 + x2) `mod` boundary

part1 :: IO ()
part1 = do
        total <- cowabunga filename sledMove31
        print total

part2 :: IO ()
part2 = do
        totals <- mapM (cowabunga filename) [sledMove11, sledMove12, sledMove31, sledMove51, sledMove71]
        let prod = product totals
        print prod

cowabunga :: FilePath -> SledMove -> IO Int
cowabunga filepath sledStart = do
        terrain <- fmap BC.lines (BC.readFile filepath)
        let dim = Dim (fromIntegral . BC.length $ head terrain) (length terrain)
        let raceLength = getRaceLength dim sledStart
        let moves = take raceLength $ trace sledStart sledStart (width dim)
        let matches = map (matchTree . getCase terrain) moves
        return $ sum matches
    where matchTree '#' = 1
          matchTree _ = 0
          getCase terrain (SledMove xx yy) = BC.index (terrain !! yy) (fromIntegral xx)
          getRaceLength dim sledMove = (len dim `quot` y sledMove) - y sledMove