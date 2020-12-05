-- | Not a brute force approach, leveraging a Trie to get complement
-- | numbers. There is a recursive function that powers both part 1 
-- | and part 2. It can manage multi-level drill down of searching 
-- | for X numbers to be equal to Total , i.e. if part 3 asked for 
-- | 4 numbers instead of 2 or 3, it would support it.

module Day1
    ( part1,
      part2
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (lines, pack, readFile, readInt)
import qualified Data.List as L (nub, sort)
import Data.Maybe ( mapMaybe )
import qualified Data.Trie as T

filename :: [Char]
filename = "./data/day1-input.txt"

convBsToInt :: ByteString -> Int
convBsToInt bs = maybe 0 fst (BS.readInt bs)
convIntToBs :: Int -> ByteString
convIntToBs = BS.pack . show

part1 :: IO ()
part1 = do 
        ls <- fmap BS.lines (BS.readFile filename)
        let intPairs = find ls 2020 1
        print $ product intPairs

part2 :: IO ()
part2 = do
        ls <- fmap BS.lines (BS.readFile filename)
        let intPairs = find ls 2020 2
        print $ product intPairs

find :: [ByteString] -> Int -> Int -> [Int]
find values = find' t
        where
            -- zip with itself, keys == values in the upcoming Trie
            zipped = zip values values 
            t = T.fromList zipped

find' :: T.Trie ByteString -> Int -> Int -> [Int]

-- Should not be necessary but just to be safe.
find' _ _ 0 = [] 

-- This is the bottom level when we need to look for matching numbers
-- with number differences coming from the total of higher levels. 
find' t total 1 = intPairs 
        where
            diff n = total - n
            keys = map convBsToInt (T.keys t)
            diffs = map diff keys
            bsDiffs = map convIntToBs diffs
            pairs = mapMaybe (`T.lookup` t) bsDiffs
            intPairs = map convBsToInt pairs

-- Higher levels only need to build new total numbers by subtracting
-- current total with value numbers, then send the updated total down 
-- to lower levels.
find' t total level = unique $ concatMap recurse (T.elems t)
        where
            unique = L.nub . L.sort
            newTotal n = total - convBsToInt n
            recurse n = find' t (newTotal n) (level - 1)
