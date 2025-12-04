-- Advent of Code 2025 - Day 4 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: The number of rolls of paper that can be accessed by a forklift is: 1370
--  Part two: The number of rolls of paper that can be accessed by a forklift is: 8437
--
-- (cl) by Arno Jacobs, 2025-12-04

module AoC2025d04ab where

data Part = PartOne | PartTwo deriving Eq

filename :: String
filename = "data/inputDay04_2025.txt"

type Element        = Char
type PaperRollGrid  = [[Element]]
type Counter        = Int
type CountGrid      = [[Counter]]
type Position       = (Int,Int)

paperRoll :: Element
paperRoll = '@'

cleanSpace :: Element
cleanSpace = '.'

countAdjacentRolls :: PaperRollGrid -> CountGrid
countAdjacentRolls grid = [[ countAdjacentRolls' grid (x,y) (maxX,maxY) | x <- [0..maxX]] | y <- [0..maxY]] 
    where
        maxX = length (grid !! 0) - 1
        maxY = length grid - 1
    --  countAdjacentRolls' :: PaperRollGrid -> Position -> Position -> Counter
        countAdjacentRolls' grid (x,y) (maxX,maxY)
            | grid !! y !! x /= paperRoll   = 0
            | otherwise                     =
                sum [ countAdjacentRolls'' grid (x+dx,y+dy) (maxX,maxY) | dx <- [-1..1], dy <- [-1..1] ]
            where            
            --  countAdjacentRolls'' :: PaperRollGrid -> Position -> Position -> Counter
                countAdjacentRolls'' grid (dx,dy) (maxX,maxY)
                    |   dx < 0      ||  dy < 0 
                    ||  dx > maxX   ||  dy > maxY       = 0
                    |   grid !! dy !! dx == paperRoll   = 1 
                    |   otherwise                       = 0

removePaperRolls :: CountGrid -> PaperRollGrid
removePaperRolls countGrid = [[ removePaperRoll (countGrid !! y !! x) | x <- [0..maxX]] | y <- [0..maxY]] 
    where
        maxX = length (countGrid !! 0) - 1
        maxY = length countGrid - 1
    --  removePaperRoll :: Counter -> Element
        removePaperRoll prc | prc < 5   = cleanSpace
                            | otherwise = paperRoll

workGrid :: Part -> Counter -> PaperRollGrid -> Counter
workGrid part cnt grid  | part == PartOne   = canBeMoved
                        | canBeMoved == 0   = cnt
                        | otherwise         = workGrid part (canBeMoved + cnt) nextGrid
    where
        countGrid   = countAdjacentRolls grid
        canBeMoved  = (length . filter canAccess . concat) countGrid
        nextGrid    = removePaperRolls countGrid
    --  canAccess :: Counter -> Bool
        canAccess prc = prc > 0 && prc < 5  
        --  < 5 because the code is also counting the paper roll that can be accessed itself


main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 4  (Haskell)"
            day4 <- lines <$> readFile filename
            putStr "Part one: The number of rolls of paper that can be accessed by a forklift is: "
            print $ workGrid PartOne 0 day4
            putStr "Part two: The number of rolls of paper that can be accessed by a forklift is: "
            print $ workGrid PartTwo 0 day4
            putStrLn "0K.\n"

-- End of code


