-- Advent of Code 2025 - Day 11 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one:   The number of different paths leading from you to out is:   508
--  Part two:   The number of different paths visiting both dac and fft is: 315116216513280
--
--
--  The compiled code needs 32Gb RAM and almost 5 minutes runtime on my M4 !
--

-- (cl) by Arno Jacobs, 2025-12-14

module AoC2025d11ab where

import Data.List.Split  (splitOn)
-- I see no difference in memory use between the 'Word16' or 'Int16' use 
-- and 'Int' which is 64 bits. -- Need packing...

type Index = Int

filename :: String
filename = "data/inputDay11_2025.txt"

parseConnections :: String -> (String,[String])
parseConnections xs = (ds,splitOn " " oss)
    where 
        (ds:oss:_) = splitOn ": " xs

-- quick sort but only store unique elements
-- So: [2,4,3,1,1,2,3,5,2,6,3,4] -> [1,2,3,4,5,6]
usort :: Ord a => [a] -> [a]
usort []     = []
usort (e:rl) = usort smaller ++ [e] ++ usort bigger
                where
                    smaller = filter (<e) rl
                    bigger  = filter (>e) rl 

backwards :: (Eq a, Ord a) => [(a,[a])] -> [(a,[a])] 
backwards nws = backwards' toss nws
    where
        toss = (usort . concat . map snd) nws 
        --
        backwards' []       _    = []
        backwards' (ts:rts) nws  = 
            [(ts,[ fs | (fs,tss) <- nws, elem ts tss ])] ++ backwards' rts nws

getIndexes :: (Eq a, Ord a) => [([a],[[a]])] -> [(Index,[a])] 
getIndexes nws = zip [0..] sns
    where
        ns1 = map fst nws
        ns2 = concat $ map snd nws
        sns = usort $ ns1 ++ ns2

getIndex :: Eq a => [(Index,[a])] -> [a] -> Index
getIndex ixs s = fromIntegral $ head $ [ ix | (ix,ns) <- ixs, ns == s ] ++ [-1]

token2Indexes :: (Eq a, Ord a) => [([a],[[a]])] -> [(Index,[Index])] 
token2Indexes nws = [ (getIndex ixs s, map (getIndex ixs) sl) | (s,sl) <- nws ]
    where
        ixs = getIndexes nws

travel :: Index -> Index -> [(Index,[Index])] -> [Int]
travel start = travel' [start] 
    where
    --  travel' :: [Index] -> Index -> [(Index,[Index])] -> [Int]
        travel' starts end network    
            | nextSteps == []   = [endCount]
            | endCount == 0     = travel' nextSteps end network
            | otherwise         = [endCount] ++ travel' nextSteps end network
                where
                    endCount    = length $ filter (==end) starts
                    nextSteps   = (concat . map snd . concat . 
                                    map (\start -> (filter (\(d,_) -> d == start) network))) 
                                        (filter (/=end) starts)

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 11  (Haskell)"
            day11 <- map parseConnections <$> lines <$> readFile filename
            --
            let tokenIxs    = token2Indexes day11
            let backwardIxs = backwards tokenIxs
            --
            let ixs     = getIndexes day11
            let iYou    = getIndex ixs "you"
            let iSvr    = getIndex ixs "svr"
            let iFFT    = getIndex ixs "fft"
            let iDac    = getIndex ixs "dac"
            let iOut    = getIndex ixs "out"

            --  part one ---------------------------------------------------------------------
            putStr "Part one: The number of different paths leading from you to out is:   "
            print $ sum $ travel iYou iOut tokenIxs
            
            --  part two ---------------------------------------------------------------------
            --      Split paths and work from dac -> out, dac -> fft {backward}
            --      and from fft -> svr {backward}
            --      The total number of paths is the product of the three
            --
            let pathsDac2Out    = sum $ travel iDac iOut tokenIxs
            --      For optimize on speed and memory usage, the first 5 are enough
            let pathsDac2FFT    = sum $ take 5 $ travel iFFT iDac tokenIxs
            let pathsFFT2Svr    = sum $ travel iFFT iSvr backwardIxs
            let paths           = product [ pathsDac2Out, pathsDac2FFT, pathsFFT2Svr ]
            putStr "Part two: The number of different paths visiting both dac and fft is: "
            print paths
            
            putStrLn "0K.\n"
 

-- End of code

