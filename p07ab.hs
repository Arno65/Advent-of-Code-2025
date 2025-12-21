-- Advent of Code 2025 - Day 7 part One and Two
-- Solutions in Haskell
-- https://adventofcode.com/2025/day/7
-- (Ter leering ende vermaeck...)
--
--  Part one: The number of times the beam is split is: 1622
--  Part two: The number of different timelines is:     10357305916520
--
--
-- (cl) by Arno Jacobs, 2025-12-07

module AoC2025d07ab where

filename :: String
filename = "data/inputDay07_2025.txt"

cStart :: Char
cStart = 'S'

cSplitter :: Char
cSplitter = '^'

cRay :: Char 
cRay = '|'

----------------------------------------------------------------
-- Part One

placeRays :: String -> [Int] -> String
placeRays ls []         = ls
placeRays ls (rp:rrps)  = placeRays (placeRay ls rp) rrps

placeRay :: String -> Int -> String
placeRay ls rp = take rp ls ++ [cRay] ++ drop (rp+1) ls

elementLocations :: Char -> String -> [Int]
elementLocations e ls = [ ix | (ix,ec) <- zip [0..] ls, ec == e ]

countBeamSplits :: [Int] -> [Int] -> Int
countBeamSplits rls sls = length [ 1 | sl <- sls, elem sl rls ]

----------------------------------------------------------------
-- Part Two

nextSplitPaths :: [Int] ->  [Int] ->  [Int] ->  [Int] 
nextSplitPaths rls sls paths = addNewPaths paths newPaths
    where
        newPaths = [ sl | sl <- sls, elem sl rls ]

addNewPaths :: [Int] -> [Int] -> [Int]
addNewPaths paths []        = paths
addNewPaths paths (np:rnps) = addNewPaths nextPath rnps
    where
        splitPoint      = paths !! np
        plusPathLeft    = splitPoint + paths !! (np-1)
        plusPathRight   = splitPoint + paths !! (np+1)
        nextPath        = take (np-1) paths ++ [ plusPathLeft, 0, plusPathRight ] ++ drop (np+2) paths

splitBeams :: String -> [Int] -> [Int] -> String
splitBeams nrl []        _      = nrl
splitBeams nrl (rl:rrls) sls    | elem rl sls   = splitBeams split   rrls sls
                                | otherwise     = splitBeams through rrls sls
    where
        split   = placeRays nrl [rl-1,rl+1]
        through = placeRay nrl rl

-- First get the start position and work the rays
--
workRays :: [String] -> ([Int],[Int])
workRays []         = ([],[])       -- some range checking
workRays (_:[])     = ([],[])       -- in need of at least two strings
workRays (l:nl:rls) = workRays' (rl:rls) [] paths
    where
        lln     = length l
        sl      = length $ takeWhile (/=cStart) l
        rl      = placeRay nl sl
        paths   = take lln $ replicate sl 0 ++ [1] ++ replicate lln 0 
        --
        workRays' []            beamSplits paths = (beamSplits,paths) 
        workRays' (_:[])        beamSplits paths = (beamSplits,paths) 
        workRays' (l:nl:nrl:ls) beamSplits paths = workRays' (newrl:ls) nextBeamSplits nextPaths 
            where
                rls = elementLocations cRay l           -- ray locations
                sls = elementLocations cSplitter nl     -- splitter locations
                --
                nextBeamSplits  = beamSplits ++ [countBeamSplits rls sls]
                newrl           = splitBeams nrl rls sls
                nextPaths       = nextSplitPaths rls sls paths

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 7  (Haskell)"
            day7 <- lines <$> readFile filename
            putStr "Part one: The number of times the beam is split is: "
            print . sum . fst $ workRays day7
            putStr "Part two: The number of different timelines is:     "
            print . sum . snd $ workRays day7
            putStrLn "0K.\n"

-- End of code
