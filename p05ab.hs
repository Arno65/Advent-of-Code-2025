-- Advent of Code 2025 - Day 5 part One and Two
-- Solutions in Haskell
-- https://adventofcode.com/2025/day/5
-- (Ter leering ende vermaeck...)
--
--  Part one: The number of fresh ingredients available is:         840
--  Part two: The number of IDs that are considered to be fresh is: 359913027576322
--
-- (cl) by Arno Jacobs, 2025-12-05

module AoC2025d05ab where

import Data.List        (sort)
import Data.List.Split  (splitOn)

type Range  = (Int,Int)
type Ranges = [Range]
type IDs    = [Int]

filename :: String
filename = "data/inputDay05_2025.txt"

parseIDs :: [String] -> (Ranges,IDs)
parseIDs dta = (getRanges dta,map read ds) 
    where
        getRanges (l:ls)    | length l > 1  = [getRange l] ++ getRanges ls
                            | otherwise     = []
        ds = tail $ dropWhile (\l -> length l > 1) dta
        getRange l = (read f, read t)
            where
                (f:t:_) = splitOn "-" l

-- All simple brute force code -- but still fast...
--
countFreshIDs :: Ranges -> IDs -> Int
countFreshIDs ranges = length . filter (inRange ranges) 
    
inRange :: Ranges -> Int -> Bool 
inRange []         _    = False
inRange ((f,t):rs) i    | i < f || i > t    = inRange rs i     
                        | otherwise         = True 


uniqueRanges :: Ranges -> Ranges
uniqueRanges rs     | rs == urs = urs
                    | otherwise = uniqueRanges urs
    where
        urs = uniqueRanges' [] rs
        --
        uniqueRanges' urs []        = urs
        uniqueRanges' urs (r:rs)    = uniqueRanges' nurs rs
            where
                nurs = sort $ overlapRanges urs r
                --
                overlapRanges []             (f,t)  = [(f,t)]
                overlapRanges ((fr,tr):rurs) (f,t)  
                    | f > tr  || t < fr     = [(fr,tr)] ++ overlapRanges rurs (f,t)
                    | f >= fr && t <= tr    = [(fr,tr)] ++ rurs
                    | fr >= f && tr <= t    = [(f,t)]   ++ rurs
                    | f >= fr && tr <= t    = [(fr,t)]  ++ rurs
                    | fr >= f && t <= tr    = [(f,tr)]  ++ rurs
                    | otherwise             = [(fr,tr)] ++ overlapRanges rurs (f,t)

countAllFreshIDs :: Ranges -> Int
countAllFreshIDs = sum . map countIDs 
    where
        countIDs (f,t) = t - f + 1

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 5  (Haskell)"
            day5 <- lines <$> readFile filename
            let (ranges,ids) = parseIDs day5
            putStr "Part one: The number of fresh ingredients available is:         "
            print $ countFreshIDs ranges ids
            putStr "Part two: The number of IDs that are considered to be fresh is: "
            print $ countAllFreshIDs $ uniqueRanges ranges
            putStrLn "0K.\n"

            

-- End of code


