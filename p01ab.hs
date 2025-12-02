-- Advent of Code 2025 - Day 1 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The password to open the door for part one: 1147
-- The password to open the door for part two: 6789
--
-- (cl) by Arno Jacobs, 2025-12-01

module AoC2025d01ab where


-- Some initials
filename :: String
filename = "data/inputDay01_2025.txt"

startDial :: Int
startDial = 50 

dialResolution :: Int
dialResolution = 100

parseRotations :: [String] -> [Int]
parseRotations = map parseRotation
    where
        parseRotation ('R':n)   = read n
        parseRotation (_:n)     = negate $ read n 

countOnZero :: [Int] -> Int
countOnZero rs = countOnZero' 0 startDial rs
    where
        countOnZero' c _ []     = c 
        countOnZero' c p (r:rs) | np == 0   = countOnZero' (c+1) 0  rs
                                | otherwise = countOnZero'  c    np rs        
            where
                np = mod (p+r) dialResolution


countClicksOnZero :: [Int] -> Int
countClicksOnZero rs = countClicksOnZero' 0 startDial rs
    where
        countClicksOnZero' c _ []       = c
        countClicksOnZero' c p (r:rs)   | p == 0    = countClicksOnZero' (c+cp0) np rs
                                        | otherwise = countClicksOnZero' (c+cc0) np rs
            where
                np  = mod (p+r) dialResolution
                pp  = if r < 0 then (dialResolution-p) else p
                rp  = abs r
                cc0 = countClicksOnZero'' pp rp
                cp0 = div rp dialResolution
                -- count the multiple click (for >100+ rotations)
                countClicksOnZero'' p r = mc + div (p+mr) dialResolution
                    where
                        (mc,mr) = divMod r dialResolution


main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 1  (Haskell)"
            day1 <- parseRotations <$> lines <$> readFile filename
            putStr "Part one: The password to open the door for part one: "
            print $ countOnZero day1
            putStr "Part two: The password to open the door for part two: "
            print $ countClicksOnZero day1            
            putStrLn "0K.\n"


-- End of code
