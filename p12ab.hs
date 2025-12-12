-- Advent of Code 2025 - Day 12 - last quest . . . only one part
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  The number of regions that fit all of the presents listed is: 593
--
--
--- !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!!  ---
--  This is the version without any 'try-place packages on the grids' code
--  The program only seems to need to check the area size and compare it 
--  to the size needed to place all packages, 
--  . . . regardless of placement and orientation
--  But also, this code won't work for the example test set
--- !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!! !!!!!!!!!!!  ---
--
-- (cl) by Arno Jacobs, 2025-12-12

-- 
module AoC2025d12ab where

import Data.List.Split  (splitOn)

type PackageSizes   = [Int]
type Size           = (Int,Int)
type Presents       = [Int]
type Region         = (Size,Presents)
type Regions        = [Region]

filename :: String
filename = "data/inputDay12_2025.txt"


getAllPackageSizes :: [String] -> PackageSizes
getAllPackageSizes []       = []
getAllPackageSizes (x:xs)   | x == ""       = getAllPackageSizes xs
                            | elem 'x' x    = []
                            | elem ':' x    = [packageSize xs] ++ getAllPackageSizes (next xs)
    where 
        packageSize = length . filter (\p -> p == '#') . concat . takeWhile (/="")
        next        = tail . dropWhile (/="") 

getRegions :: [String] -> Regions
getRegions []       = []
getRegions (x:xs)   | elem 'x' x    = [getRegion x] ++ getRegions xs
                    | otherwise     = getRegions xs
    where
        getRegion rs = ((areaX,areaY),packages)
            where
                (areaX:areaY:_) = (map read . splitOn "x" . takeWhile (/=':')) rs
                packages        = (map read . splitOn " " . dropWhile (==' ') . tail . dropWhile (/=':')) rs

filterOnSize :: Regions -> PackageSizes -> Regions
filterOnSize rs packageSizes = 
    [ (area,packages) |    
            (area,packages) <- rs, 
            let availableArea       = fst area * snd area,
            let minimalNeededArea   = innerProduct packages packageSizes,
            availableArea >= minimalNeededArea ]
        where
            innerProduct v1 v2 = sum [ e1 * e2 | (e1,e2) <- zip v1 v2 ]


main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 12  (Haskell)"
            day12 <- lines <$> readFile filename
            let shapeSizes  = getAllPackageSizes day12
            let regions     = getRegions day12
            let fitOnSize   = filterOnSize regions shapeSizes 
            putStr "The number of regions that fit all of the presents listed is: "
            print $ length fitOnSize
            putStrLn "0K.\n"
 

-- End of code
