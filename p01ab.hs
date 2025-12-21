-- Advent of Code 2025 - Day 1 part One and Two
-- Solutions in Haskell
-- https://adventofcode.com/2025/day/1
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
countOnZero = countOnZero' 0 startDial
    where
        countOnZero' zerosCount _             []                            = zerosCount 
        countOnZero' zerosCount dialPosition (rotation:remainingRotations)    
            | nextDialPosition == 0 = countOnZero' (zerosCount+1) 0                remainingRotations
            | otherwise             = countOnZero'  zerosCount    nextDialPosition remainingRotations
                where
                    nextDialPosition = mod (dialPosition + rotation) dialResolution


countClicksOnZero :: [Int] -> Int
countClicksOnZero = countClicksOnZero' 0 startDial
    where
        countClicksOnZero' clicks _ []       = clicks
        countClicksOnZero' clicks dialPosition (rotation:remainingRotations)
            | dialPosition == 0 = countClicksOnZero' (clicks + nextClicksAt0) nextDialPosition remainingRotations
            | otherwise         = countClicksOnZero' (clicks + nextClicksOn0) nextDialPosition remainingRotations
                where
                    nextDialPosition    = mod (dialPosition + rotation) dialResolution
                    positionHelper      = if rotation < 0 then (dialResolution - dialPosition) else dialPosition
                    positiveRotation    = abs rotation
                    nextClicksOn0       = countClicksOnZero'' positionHelper positiveRotation
                    nextClicksAt0       = div positiveRotation dialResolution
                    -- count the multiple click (for >100+ rotations)
                    countClicksOnZero'' position rotation = multiClicks + div (position + remainderPosition) dialResolution
                        where
                            (multiClicks,remainderPosition) = divMod rotation dialResolution

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 1  (Haskell)"
            day1 <- parseRotations <$> lines <$> readFile filename
            putStr "The password to open the door for part one: "
            print $ countOnZero day1
            putStr "The password to open the door for part two: "
            print $ countClicksOnZero day1            
            putStrLn "0K.\n"


-- End of code
