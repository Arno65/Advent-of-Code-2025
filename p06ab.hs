-- Advent of Code 2025 - Day 6 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: The grand total is: 4648618073226
--  Part two: The grand total is: 7329921182115
--
--
-- (cl) by Arno Jacobs, 2025-12-06

module AoC2025d06ab where

import Data.List        (transpose)
import Data.List.Split  (splitOn)

data Operator = Add | Multiply deriving (Eq,Show)

type Operators = [Operator]


filename :: String
filename = "data/inputDay06_2025.txt"


splitData :: String -> [String]
splitData = filter (/= "") . splitOn " "

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces xs | last xs == ' '    = (removeSpaces . init) xs
                | otherwise         = dropWhile (==' ') xs 

parseProblems :: [String] -> ([[Int]],Operators)
parseProblems dta = (numbers,operators)
    where
        numbers     = map (map read . splitData) (init dta)
        operators   = parseOperators (last dta)

parseOperators :: String -> Operators
parseOperators = map parseOperator . splitData
    where
        parseOperator ('+':_)   = Add
        parseOperator  _        = Multiply

eval :: [[Int]] -> Operators -> Int
eval nrs = sum . map evalOne . zip nrs
    where
        evalOne ([]        ,Add     )   = 0     -- unit values
        evalOne ([]        ,Multiply)   = 1
        evalOne ((nr:rnrs) ,Add     )   = nr + evalOne (rnrs,Add)
        evalOne ((nr:rnrs) ,Multiply)   = nr * evalOne (rnrs,Multiply)

workPartOne :: [String] -> Int
workPartOne dta = eval nrs ops
    where
        (nrls,ops)  = parseProblems dta
        nrs         = transpose nrls


getGroupLengths :: String -> [Int]
getGroupLengths []      = []
getGroupLengths opsl    | hln == gln    = [gln+1]
                        | otherwise     = [gln] ++ getGroupLengths nopsl
    where
        nopsl   = (tail . dropWhile (==' ')) opsl 
        gln     = (length . takeWhile (==' ')) nopsl
        hln     = length nopsl

-- parsing numbers for part two
parseNumbers :: [String] -> [[Int]]
parseNumbers dta = groupNumbers nrsl glls
        where
            glls    = (getGroupLengths . last) dta
            nrsl    = (map read . filter (/="") . map removeSpaces . transpose . init) dta
            --
            groupNumbers [] _           = []
            groupNumbers _ []           = []
            groupNumbers nrsl (gl:rgl)  = [take gl nrsl] ++ groupNumbers (drop gl nrsl) rgl 

workPartTwo :: [String] -> Int
workPartTwo dta = eval nrsg ops
    where
        nrsg = parseNumbers dta
        ops  = (parseOperators . last) dta


main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 6  (Haskell)"
            day6 <- lines <$> readFile filename
            putStr "Part one: The grand total is: "
            print $ workPartOne day6
            putStr "Part two: The grand total is: "
            print $ workPartTwo day6
            putStrLn "0K.\n"

-- End of code

