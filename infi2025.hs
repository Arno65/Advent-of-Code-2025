-- Advent of Code 2025 - Infi
-- Solutions in Haskell (GHC 9.6.7.)
-- https://aoc.infi.nl/2025
-- (Ter leering ende vermaeck...)
--
--  Part one: The number of trees cut after 256 days is: 4330
--  Part two: The number of trees cut after 256 days is: 23142
--
-- (cl) by Arno Jacobs, 2025-12-07
-- 

-- 
module AoC2025_Infi where

filename :: String
filename = "../data/inputInfi2025.txt"

data Location   = Tree Int | Empty | NotOnGrid  deriving Eq
data LightState = Light | Dark                  deriving Eq
data Task       = One | Two                     deriving Eq

-- Show instances for debug help
instance Show Location where
    show (Tree n)   = " " ++ (show n) ++ " "
    show Empty      = " - "
    show NotOnGrid  = "   "

instance Show LightState where
    show Light = " LIGHT "
    show Dark  = " dark  "

showGrid :: Grid -> IO()
showGrid []     = return ()
showGrid (l:ls) = do    putStrLn $ concat $ map show l
                        showGrid ls

type Line           = [Location]
type Grid           = [Line]
type LightStates    = [[LightState]]
type Coordinate     = (Int,Int)
type Coordinates    = [Coordinate]

days :: Int
days = 256 --  day: 3, 8, 256  -->  I (4,13,529)    II (5,21,1117)

isTree :: Location -> Bool
isTree (Tree _) = True
isTree  _       = False

isEmpty :: Location -> Bool
isEmpty Empty   = True
isEmpty _       = False

isLight :: LightState -> Bool
isLight Light   = True
isLight _       = False

-- Parse the data and put the hexagonal grid in a square grid (2D matrix)
-- This grid can be rotated more easily.
-- 
parseGrid :: [String] -> Grid
parseGrid grid = map parseLine $ zip [0..] $ (init . tail) grid

parseLine :: (Int,String) -> Line
parseLine (ix,line) | even ix   = parseLine' line
                    | otherwise = [NotOnGrid] ++ 
                                    init (parseLine' (dropWhile (/= '/') line))
    where
        parseLine' []       = []
        parseLine' ('/':[]) = []
        parseLine' (c:cs)   | c == '/'  = getLocation cs ++ parseLine' (tail cs)
                            | otherwise = parseLine' cs
        --
        getLocation (' ':_) = [Empty]                   ++ [NotOnGrid]
        getLocation tree    = [Tree (read [head tree])] ++ [NotOnGrid]

-- Rotate a 2D matrix of arbitrary type anti clockwise 
rotateAntiClockwise :: [[a]] -> [[a]]
rotateAntiClockwise grid = 
    [[ grid !! x !! y  | x <- [0..maxY] ] | yh <- [0..maxX ], let y = maxX-yh ]
        where
            maxX = length (grid !! 0) - 1
            maxY = length  grid       - 1

-- Backwise rotation -- only used as preparation for printing
rotateBack :: Int -> [[a]] -> [[a]]
rotateBack days = rotateBack' (mod (negate days) 4)
    where
        rotateBack' 0 mx = mx
        rotateBack' i mx = rotateBack' (i-1) (rotateAntiClockwise mx)

workTrees :: Task -> Grid -> Int -> Int
workTrees = workTrees' 0 1 
    where
        workTrees' cutTrees day task grid days              -- work the work - day by day
            | day > days    = cutTrees
            | otherwise     = 
                workTrees' (cutTrees + nextCutTrees) (day+1) task nextGrid days
                    where
                        (nextCutTrees,nextGrid) =
                            (countAndCutTrees               -- cut the large trees and count them
                            . (growTrees task day)          -- grow function
                            . rotateAntiClockwise) grid     -- rotate and always 'work' from the west side

-- Mark all locations in the light  -  with or without the indirect light from fluorescence
-- Create Seeds by rules on locations in the light
-- Grow all elements in the light with 1
-- This - location by location
growTrees :: Task -> Int -> Grid -> Grid
growTrees task day grid = mergeGrids nextTrees newSeeds
    where
        -- First mark all spots that are in the light -- all light from the west
        directLight     = map (inTheLight 0) grid           -- direct light from the train
        indirectLight   = fluorescention grid directLight   -- indirect light from fluorescence
                                                                -- merge both markings if needed
        marked          = if task == One then directLight else mergeLights directLight indirectLight
        newSeeds        = plantNewSeeds day marked grid     -- plant new seeds - by the rules
        nextTrees       = growTheTrees marked grid          -- grow all trees by one

-- function for merging direct and indirect light
mergeLights :: LightStates -> LightStates -> LightStates
mergeLights ls1 []  = ls1
mergeLights []  ls2 = ls2
mergeLights ls1 ls2 = [[ mergeLight l1 l2 | (l1,l2) <- zip ll1 ll2 ] |  (ll1,ll2) <- zip ls1 ls2 ]
    where
        mergeLight l1 l2    | l1 == Light || l2 == Light    = Light
                            | otherwise                     = Dark

-- function for detecting direct light from the train
-- Because of rotation the direct light goed from left to right 
inTheLight :: Int -> Line -> [LightState]
inTheLight _   []       = []
inTheLight top (l:ls)   
    | l == NotOnGrid        = [Dark]  ++ inTheLight top ls
    | top == 0 && bottom    = [Light] ++ inTheLight top ls
    | l == Empty            = [Dark]  ++ inTheLight top ls
    | treeHeight >= top     = [Light] ++ inTheLight (treeHeight+1) ls
    | otherwise             = [Dark]  ++ inTheLight top ls
        where
            (Tree treeHeight)   = l
            bottom              = l == Empty || treeHeight == 0

-- function for detecting indirect light from the the lit trees
fluorescention :: Grid -> LightStates -> LightStates
fluorescention []   _           = []
fluorescention grid directLight = [[ setLight newLights (x,y) | x <- [0..maxX]] | y <- [0..maxY]]
    where
        maxX = length (grid !! 0) - 1
        maxY = length grid        - 1
        -- helper for creating a light states grid
        setLight lights coordinate  | elem coordinate lights    = Light
                                    | otherwise                 = Dark
        -- Create a list of all locations that receive any form of indirect light
        -- Do this for all locations - checking the most right column is unnecessary
        newLights = concat [ fluorescention' grid directLight (x,y) (maxX,maxY) | 
                                x <- [0..maxX-1], 
                                y <- [0..maxY]]
        -- creation of indirect light only happens from trees that receive direct light
        -- Select those trees and work from them 
        fluorescention' grid directLight (x,y) maxXY
            |   isTree (grid !! y !! x) 
            &&  cth > 0 
            &&  isLight (directLight !! y !! x)     = fluorescention'' grid directLight (x,y) maxXY
            | otherwise                             = []
                where
                    (Tree cth)  = grid !! y !! x
        -- Create the two paths, seen from the west, one goes up, the other one down (or left and right)
        fluorescention'' grid directLight (x,y) maxXY = leftCoordinates ++ rightCoordinates
            where
                (Tree cth)          = grid !! y !! x    -- This is the top level of indirect light
                leftCoordinates     = fluorescentionRay grid (x,y) (1,-1) maxXY 0 cth 
                rightCoordinates    = fluorescentionRay grid (x,y) (1, 1) maxXY 0 cth

-- The function with the conditions for the indirect light
fluorescentionRay :: Grid -> Coordinate -> Coordinate -> Coordinate -> Int -> Int -> Coordinates
fluorescentionRay grid (x,y) (dx,dy) (maxX,maxY) cBottom mTreeHeight 
    |   cx > maxX || cy < 0 || cy > maxY        = []            -- a range check
    |   isTree e && th >= mTreeHeight           = [(cx,cy)]     -- a big tree in the path of the indirect light
    |   (isEmpty e && cBottom == 0)                             -- light a the bottom over empty space or seeds
    ||  (isTree  e && cBottom == 0 && th == 0)  = [(cx,cy)] ++ fluorescentionRay grid (cx,cy) (dx,dy) (maxX,maxY) cBottom mTreeHeight
    |   isTree e && th > cBottom                                -- light over the top of a tree 
    &&  th < mTreeHeight                        = [(cx,cy)] ++ fluorescentionRay grid (cx,cy) (dx,dy) (maxX,maxY) th      mTreeHeight
                                                                -- all other conditions
    |   otherwise                               =              fluorescentionRay grid (cx,cy) (dx,dy) (maxX,maxY) cBottom mTreeHeight
            where
                (cx,cy)     = (x+dx,y+dy)
                cxyl        = [(cx,cy)]
                e           = grid !! cy !! cx
                (Tree th)   = e

-- The planting of new seeds - accoring to given conditions
plantNewSeeds :: Int -> LightStates -> Grid -> Grid
plantNewSeeds day marked grid = 
    [[ plantNewSeed day m (x,y) grid    | (x,m)  <- zip [0..] ml ]
                                        | (y,ml) <- zip [0..] marked ]

plantNewSeed :: Int -> LightState -> Coordinate -> Grid -> Location
plantNewSeed _   Dark _     _       = Empty                     -- the has to be light
plantNewSeed day _    (x,y) grid    | notEmpty      = Empty
                                    | bigTrees >= 2 = Tree 0
                                    | otherwise     = Empty
    where
        tree0       = grid !! y !! x
        notEmpty    = not $ isEmpty tree0
        -- horizontal or vertical orientation after rotation
        -- This has to be done bacause of the hexagonal input grid
        (dx,dy)     = if even day then (0,2) else (2,0) 
        bigTree1    = readSafeBigTree grid (x-1,y-1)    -- top left
        bigTree2    = readSafeBigTree grid (x-1,y+1)    -- bottom left
        bigTree3    = readSafeBigTree grid (x+1,y-1)    -- top right
        bigTree4    = readSafeBigTree grid (x+1,y+1)    -- bottom right
        bigTree5    = readSafeBigTree grid (x-dx,y-dy)  -- right up
        bigTree6    = readSafeBigTree grid (x+dx,y+dy)  -- right down
        bigTrees    = sum [ bigTree1, bigTree2, bigTree3,
                            bigTree4, bigTree5, bigTree6 ]

-- Read safe -- a read including range checks
-- Count the locations with trees larger or equal than 2
readSafeBigTree :: Grid -> Coordinate -> Int
readSafeBigTree []   _      = 0
readSafeBigTree grid (x,y)  |   x < 0       ||  y < 0
                            ||  x >= maxX   ||  y >= maxY   = 0
                            | th >= 2                       = 1
                            | otherwise                     = 0
    where
        maxX = length (grid !! 0)
        maxY = length  grid
        th   = getTreeHeight (grid !! y !! x)
        --
        getTreeHeight (Tree h)  = h
        getTreeHeight  _        = -1

-- Map over the complete grid and grow all trees that receive light by 1
growTheTrees :: LightStates -> Grid -> Grid
growTheTrees marked grid =
    [[ growOneTree l m | (l,m) <- zip gl ml ] | (gl,ml) <- zip grid marked ]
        where
            growOneTree (Tree h) Light  = Tree (h+1)
            growOneTree  l       _      = l

-- Place the New Seeds of grid2 over grid1
mergeGrids :: Grid -> Grid -> Grid
mergeGrids grid1 grid2 =
    [[ mergeLocation l1 l2  | (l1,l2)   <- zip gl1   gl2 ]
                            | (gl1,gl2) <- zip grid1 grid2 ]
        where
            mergeLocation l1 l2 | isSeed l2 = l2
                                | otherwise = l1
                where
                    isSeed (Tree 0) = True
                    isSeed  _       = False

-- Map over the grid, cut all trees of size 5 and count them
countAndCutTrees :: Grid -> (Int,Grid)
countAndCutTrees grid = (cts,cutGrid)
    where
        cts     = countTopTree $ concat grid
        cutGrid = [[ cut5 p | p <- ls ] | ls <- grid  ]
        -- 
        cut5 (Tree 5)   = Empty
        cut5 p          = p
        --
        countTopTree []     = 0
        countTopTree (l:ls) | l == (Tree 5) = 1 +   countTopTree ls
                            | otherwise     =       countTopTree ls


main :: IO ()
main = do   putStrLn "Advent of Code 2025 - Infi  (Haskell)"
            putStr   "Part one: The number of trees cut after "
            putStr $ show days ++ " days is: "
            grid <- parseGrid <$> lines <$> readFile filename
            print $ workTrees One grid days
            putStr   "Part two: The number of trees cut after "
            putStr $ show days ++ " days is: "
            print $ workTrees Two grid days
            putStrLn "0K.\n"


-- End of code
