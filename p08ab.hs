-- Advent of Code 2025 - Day 8 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: The product of the sizes of the three largest circuits is:     135169
--  Part two: The product of the X coordinates of the last two junctions is: 302133440
--
--
-- (cl) by Arno Jacobs, 2025-12-08


-- module AoC2025d08ab where

import Data.List.Split  (splitOn)

connectPairs :: Int
connectPairs = 1000

filename :: String
filename = "data/inputDay08_2025.txt"

-- helper
-- quick sort but only store unique elements
-- So: [2,4,3,1,1,2,3,5,2,6,3,4] -> [1,2,3,4,5,6]
usort :: Ord a => [a] -> [a]
usort []     = []
usort (e:rl) = usort smaller ++ [e] ++ usort bigger
                where
                    smaller = filter (<e) rl
                    bigger  = filter (>e) rl 

parsePosition :: String -> [Int]
parsePosition = map read . splitOn "," 

squareDistance :: [Int] -> [Int] -> Int
squareDistance p1 p2 = sum [ d*d | (c1,c2) <- zip p1 p2, let d = c1-c2 ]

allSquareDistances :: [[Int]] -> [(Int,(Int,Int))]
allSquareDistances pss = usort [ (squareDistance p1 p2,(i1,i2)) |   
                                    (i1,p1) <- zip [0..] pss, 
                                    (i2,p2) <- zip [0..] pss, 
                                    i2 > i1 ]

-- ---------------------------------------------------------------------------------
-- Part one 
--
createCircuits :: Int -> [(Int,Int)] -> [[Int]] -> [[Int]]
createCircuits pairs (ds:rdsp) cps  | pairs > 1 = createCircuits (pairs-1) rdsp ncs
                                    | otherwise = ncs
        where
            ncs = connect ds cps

partOne :: [[Int]] -> Int
partOne dss = (product . take 3 . reverse . usort . map length) cs
    where
        dsp = map snd $ allSquareDistances dss
        cs  = createCircuits connectPairs dsp []

-- ---------------------------------------------------------------------------------
-- Part two
--
createOneCircuit :: [[Int]] -> (Int,Int)
createOneCircuit dss = createOneCircuit' size dsp []
    where
        size    = length dss
        dsp     = map snd $ allSquareDistances dss
        --
        createOneCircuit' size []        cps = (0,0) -- this should not happen....
        createOneCircuit' size (ds:rdsp) cps 
            | size > hln || lnCs > 1    = createOneCircuit' size rdsp ncs
            | otherwise                 = ds
                where
                    ncs     = connect ds cps
                    hln     = length $ concat ncs
                    lnCs    = length ncs

connect :: (Int,Int) -> [[Int]] -> [[Int]]
connect (j1,j2) cps | mi1 == Nothing && mi2 == Nothing  = cps ++ [[j1,j2]]          -- new circuit
                    | mi1 == Nothing                    = connectAt i2 (j1,j2) cps
                    | mi2 == Nothing                    = connectAt i1 (j1,j2) cps
                    | i1 /= i2                          = reConnect (i1,i2) (j1,j2) cps 
                    | otherwise                         = connectAt i1 (j1,j2) cps  -- i1 == i2
    where
        mi1     = getConnectionIndex j1 cps
        mi2     = getConnectionIndex j2 cps
        Just i1 = mi1
        Just i2 = mi2
        --
        getConnectionIndex j cps    | cixs == []    = Nothing
                                    | otherwise     = Just (head cixs)
            where
                cixs = [ ix | (ix,ps) <- zip [0..] cps, elem j ps ]
        --
        connectAt ix (j1,j2) cps = preCps ++ [newCp] ++ postCps
            where 
                preCps  = take ix cps
                newCp   = usort $ [j1,j2] ++ cps !! ix 
                postCps = drop (ix+1) cps
        --
        reConnect (i1,i2) (j1,j2) cps = [newCp] ++ rcps
            where
                newCp       = usort $ cps !! i1 ++ cps !! i2
                rcps        = [ ps | ps <- cps, not (elem j1 ps || elem j2 ps)  ]


partTwo :: [[Int]] -> Int
partTwo ps = x1 * x2
    where      
        (j1,j2) = createOneCircuit ps
        x1      = head $ ps !! j1
        x2      = head $ ps !! j2


main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 8  (Haskell)"
            day8 <- map parsePosition <$> lines <$> readFile filename
            putStr "Part one: The product of the sizes of the three largest circuits is:     "
            print $ partOne day8            
            putStr "Part two: The product of the X coordinates of the last two junctions is: "
            print $ partTwo day8
            putStrLn "0K.\n"
            
         
-- End of code


