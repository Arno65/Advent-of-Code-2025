-- Advent of Code 2025 - Day 3 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: The total output joltage is: 17427
-- Part two: The total output joltage is: 173161749617495
--
-- (cl) by Arno Jacobs, 2025-12-03

module AoC2025d03ab where

filename :: String
filename = "data/inputDay03_2025.txt"

maxJoltage :: Int -> String -> String -> Int
maxJoltage 0  mjs _     = read mjs 
maxJoltage dp mjs js    = maxJoltage (dp-1) njs rjs 
    where
        jsp = take (1-dp+length js) js
        mc  = maximum jsp
        rjs = tail $ dropWhile (/=mc) js
        njs = mjs ++ [mc] 

partOne :: [String] -> Int
partOne = sum . map (maxJoltage 2 [])

partTwo :: [String] -> Int
partTwo = sum . map (maxJoltage 12 [])

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 3  (Haskell)"
            day3 <- lines <$> readFile filename
            putStr "Part one: The total output joltage is: "
            print $ partOne day3
            putStr "Part two: The total output joltage is: "
            print $ partTwo day3
            putStrLn "0K.\n"

-- End of code
