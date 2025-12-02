-- Advent of Code 2025 - Day 2 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: The sum of all invalid ID's is: 23534117921
-- Part two: The sum of all invalid ID's is: 31755323497
--
-- (cl) by Arno Jacobs, 2025-12-02

module AoC2025d02ab where

import Data.List.Split (splitOn)

data Count = Twice | Multiples  deriving (Eq,Show)

filename :: String
filename = "data/inputDay02_2025.txt"

parseData :: String -> [[Int]]
parseData =  map (map read . (splitOn "-")) . splitOn ","
    
collectInvalidIDs :: Count -> [Int] -> [Int]
collectInvalidIDs _ []      = []
collectInvalidIDs _ (_:[])  = [] 
collectInvalidIDs c (b:e:_) = [ i | i <- [b..e], isInvallidIDx c i ]

isInvallidIDx :: Count -> Int -> Bool
isInvallidIDx c i   |   c == Twice 
                    &&  odd hsl     = False     -- Speed up . . .
                    |   c == Twice  =      isInvallidIDx' hs hln
                    |   otherwise   = or [ isInvallidIDx' hs l   | l <- [1..hln] ]
    where
        hs  = show i
        hsl = length hs
        hln = div hsl 2
    --  isInvallidIDx' :: Eq a => [a] -> Int -> Bool
        isInvallidIDx' ids = allEqual . (splitOnLength ids)
    --  splitOnLength :: Eq a => [a] -> Int -> [[a]]
        splitOnLength [] _  = []
        splitOnLength s  l  = [split] ++ splitOnLength rest l
            where
                split = take l s
                rest  = drop l s

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (_:[]) = True        
allEqual (e:es) | e /= head es  = False
                | otherwise     = allEqual es

sumInvallidIDs :: Count -> String -> Int
sumInvallidIDs count = sum . concat . map (collectInvalidIDs count). parseData 

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 2  (Haskell)"
            day2 <- readFile filename
            putStr "Part one: The sum of all invalid ID's is: "
            print $ sumInvallidIDs Twice day2
            putStr "Part two: The sum of all invalid ID's is: "
            print $ sumInvallidIDs Multiples day2
            putStrLn "0K.\n"

-- End of code

