-- Advent of Code 2025 - Day 9 part One and Two
-- Solutions in Haskell
-- https://adventofcode.com/2025/day/9
-- (Ter leering ende vermaeck...)
--
--  Part one: The largest rectangle is: 4781546175
--  Part two: The largest rectangle is: 1573359081
--
-- (cl) by Arno Jacobs, 2025-12-19

-- module AoC2025d09ab where

import Data.List        (sort)
import Data.List.Split  (splitOn)
import Graphics.Gloss 

data Direction  = Horizontal | Vertical     deriving (Eq,Show)

type Location   = (Int,Int)
type Locations  = [Location]
type Line       = (Location,Location)
type Lines      = [Line]

filename :: String
filename = "data/inputDay09_2025.txt"

-- The positions in the input files are already sorted to 
-- create the contour of the 2d area
--
parsePosition :: String -> Location
parsePosition = readPair . splitOn "," 
    where
        readPair (pl:pr:_) = (read pl, read pr)

rectangleArea :: Location -> Location -> Int
rectangleArea (x1,y1) (x2,y2) = dx * dy
    where
        dx = 1 + abs (x1 - x2)
        dy = 1 + abs (y1 - y2)

allRectangles :: Locations -> [(Int,Locations)]
allRectangles ps = reverse $ sort [ (rectangleArea p1 p2, sps)  | 
                        ix1 <- [0..ln-2], 
                        let p1 = ps !! ix1,
                        let (p1x,p1y) = p1,
                        ix2 <- [ix1+1..ln-1],
                        let p2 = ps !! ix2,
                        let (p2x,p2y) = p2,
                        let sps = [p1,(p1x,p2y),p2,(p2x,p1y)] ]
    where
        ln = length ps

-- ------------------------------------------------------------------------------------------------------------
-- Part one 
--
maxRectangle :: Locations -> Int
maxRectangle = fst . head . allRectangles


-- ------------------------------------------------------------------------------------------------------------
-- Part two
-- 
-- Create a list with all horizontal lines from the outline of the input data (shape)
-- and a list with all the vertical lines 
-- The lines describe the closed set of the discrete locations of the line
--
getHorizontalOutlines :: Locations -> Lines
getHorizontalOutlines ls    | length ls < 2 = []
                            | otherwise = getHorizontalOutlines' $ ls ++ [head ls]
    where
        getHorizontalOutlines' []                           = []
        getHorizontalOutlines' (_:[])                       = []
        getHorizontalOutlines' ((l1x,l1y):(l2x,l2y):rls)    
            | l1y /= l2y    = getHorizontalOutlines' ((l2x,l2y):rls) 
            | otherwise     = [((lxMin,l1y),(lxMax,l1y))] ++ getHorizontalOutlines' rls
                where
                    (lxMin,lxMax) = if l1x < l2x then (l1x,l2x) else (l2x,l1x)
    
getVerticalOutlines :: Locations -> Lines
getVerticalOutlines ls  | length ls < 2 = []
                        | otherwise = getVerticalOutlines' $ ls ++ [head ls]
    where
        getVerticalOutlines' []                             = []
        getVerticalOutlines' (_:[])                         = []
        getVerticalOutlines' ((l1x,l1y):(l2x,l2y):rls)
            | l1x /= l2x    = getVerticalOutlines' ((l2x,l2y):rls)
            | otherwise     = [((l1x,lyMin),(l1x,lyMax))] ++ getVerticalOutlines' rls
                where
                    (lyMin,lyMax) = if l1y < l2y then (l1y,l2y) else (l2y,l1y)

-- Create a list with all vertical and horizontal lines from all possible rectangles
-- The lines describe the open set of the discrete locations of the line
-- 
-- First the function for the two vertical lines of one rectangle
getTwoVerticalLines :: Locations -> Lines
getTwoVerticalLines []  = []
getTwoVerticalLines rls | length rls < 4    = []
                        | cyMax - cyMin < 2 = []
                        | otherwise         = [line1,line2]
    where
        (cx1,cy1)       = rls !! 0
        (cx2,cy2)       = rls !! 2
        (cyMin,cyMax)   = if cy1 < cy2 then (cy1,cy2) else (cy2,cy1)
        -- Open set - skip the borders
        line1           = ((cx1,cyMin+1),(cx1,cyMax-1))
        line2           = ((cx2,cyMin+1),(cx2,cyMax-1))
    
getVerticalLines :: [(Int,Locations)] -> [Lines]
getVerticalLines = map (\(_,rls) -> getTwoVerticalLines rls)

-- The function for the two horzontal lines of one rectangle
getTwoHorizontalLines :: Locations -> Lines
getTwoHorizontalLines []    = []
getTwoHorizontalLines rls   | length rls < 4    = []
                            | cxMax - cxMin < 2 = []
                            | otherwise         = [line1,line2]
    where
        (cx1,cy1)       = rls !! 0
        (cx2,cy2)       = rls !! 2
        (cxMin,cxMax)   = if cx1 < cx2 then (cx1,cx2) else (cx2,cx1)
        -- Open set - skip the borders
        line1           = ((cxMin+1,cy1),(cxMax-1,cy1))
        line2           = ((cxMin+1,cy2),(cxMax-1,cy2))

getHorizontalLines :: [(Int,Locations)] -> [Lines]
getHorizontalLines = map (\(_,rls) -> getTwoHorizontalLines rls)

getDiagonalLines :: [(Int,Locations)] -> Lines
getDiagonalLines = concat . map (\(_,rls) -> getDiagonalLine rls)

getDiagonalLine :: Locations -> Lines
getDiagonalLine ls  | length ls < 4 = []
                    | otherwise     = [(lc1,lc2)]
    where
        (_:lc1:_:lc2:_) = ls

-- --------------------------------------------------------------------------------------------------------------------------
-- Intersection for lines with open ends
-- This function has much more code than needed for this program
--
hasIntersection :: Line -> Line -> Bool
hasIntersection ((l1x1,l1y1),(l1x2,l1y2)) ((l2x1,l2y1),(l2x2,l2y2))
    |   (l1x1,l1y1) == (l1x2,l1y2)
    ||  (l2x1,l2y1) == (l2x2,l2y2)              =   False   -- not two lines
    |   l1xMin == l2xMin && l1yMin == l2yMin 
    &&  l1xMax == l2xMax && l1yMax == l2yMax    =   True    -- two equal line segments
    -- The intersecion of vertial lines 
    |   vert1 && vert2 && l1x1 /= l2x1          =   False   -- non-intersecting vertical parallel lines 
    -- A horizontal and a vertical line
    |   horz1 && vert2                          =   hasIntersection ((l2x1,l2y1),(l2x2,l2y2)) ((l1x1,l1y1),(l1x2,l1y2))
    |   vert1 && horz2                          =   (l1yMin < l2y1) && (l1yMax > l2y1)
                                                &&  (l2xMin < l1x1) && (l2xMax > l1x1)
    -- swap the x-axis with the y-axis
    |   vert1 || vert2                          =   hasIntersection ((l1y1,l1x1),(l1y2,l1x2)) ((l2y1,l2x1),(l2y2,l2x2))
    
    -- In this case (after eventual swap) two horizontal lines
    |   rc1 == rc2                              =   (l1y1 == l2y1)
                                                &&  (((l2x1 > l1xMin)  && (l2x1 < l1xMax)) -- change '<' to '<=' etc. for closed sets
                                                ||   ((l2x2 > l1xMin)  && (l2x2 < l1xMax)))
    |   otherwise                               =   ((xInt > l1xMinD) && (xInt < l1xMaxD))
                                                &&  ((xInt > l2xMinD) && (xInt < l2xMaxD))
        where
            -- are there horizontal and/or vertical lines?
            horz1   = (l1y1 - l1y2) == 0
            horz2   = (l2y1 - l2y2) == 0
            vert1   = (l1x1 - l1x2) == 0
            vert2   = (l2x1 - l2x2) == 0
            -- lowest and highest x-coordinates
            l1xMin  = min l1x1 l1x2
            l1xMax  = max l1x1 l1x2
            l2xMin  = min l2x1 l2x2
            l2xMax  = max l2x1 l2x2
            --
            l1yMin  = min l1y1 l1y2
            l1yMax  = max l1y1 l1y2
            l2yMin  = min l2y1 l2y2
            l2yMax  = max l2y1 l2y2
            --
            l1xMinD = fromIntegral l1xMin :: Double
            l1xMaxD = fromIntegral l1xMax :: Double
            l2xMinD = fromIntegral l2xMin :: Double
            l2xMaxD = fromIntegral l2xMax :: Double
            -- line functions: y = rc * x + c
            l1xd1   = fromIntegral l1x1 :: Double
            l1xd2   = fromIntegral l1x2 :: Double
            l1yd1   = fromIntegral l1y1 :: Double
            l1yd2   = fromIntegral l1y2 :: Double            
            rc1     = (l1yd2 - l1yd1) / (l1xd2 - l1xd1)
            c1      = l1yd1 - rc1 * l1xd1
            --            
            l2xd1   = fromIntegral l2x1 :: Double
            l2xd2   = fromIntegral l2x2 :: Double
            l2yd1   = fromIntegral l2y1 :: Double
            l2yd2   = fromIntegral l2y2 :: Double            
            rc2     = (l2yd2 - l2yd1) / (l2xd2 - l2xd1)
            c2      = l2yd1 - rc2 * l2xd1            
            -- Intersection point for both lines if they have infinite length
            -- Only working for  rc1  not equal to  rc2
            xInt    = (c2 - c1) / (rc1 - rc2)
            yInt    = rc1 * xInt + c1 

noDiagonalIntersections :: Locations -> Line -> Bool
noDiagonalIntersections []          _           = True
noDiagonalIntersections (_:[])      _           = True
noDiagonalIntersections (l1:l2:rls) diagonal
    | hasIntersection (l1,l2) diagonal          = False
    | otherwise                                 = noDiagonalIntersections (l2:rls) diagonal

-- Check if two lines, one horizontal and one vertical, do intersect
-- In this case one line is horizontal and one vertical
-- Because one line is vertical, there is only the 'function' x = 'value' for that line
-- Work as 'intersect horizontal vertical'
--
doesIntersectHV :: Direction -> Line -> Line -> Bool
doesIntersectHV d ((l1x1,l1y1),(l1x2,l1y2)) ((l2x1,l2y1),(l2x2,l2y2))
    | l1x1 == l1x2      = doesIntersectHV d ((l2x1,l2y1),(l2x2,l2y2)) ((l1x1,l1y1),(l1x2,l1y2))   -- swap lines
    | d == Horizontal   = (not nonIntersectH) && intersectOnX && intersectOnY
    | otherwise         = (not nonIntersectV) && intersectOnX && intersectOnY
        where
            (l1xMin,l1xMax) = if l1x1 < l1x2 then (l1x1,l1x2) else (l1x2,l1x1)
            (l2yMin,l2yMax) = if l2y1 < l2y2 then (l2y1,l2y2) else (l2y2,l2y1)
            intersectOnX = l2yMin <= l1y1 && l2yMax >= l1y1
            intersectOnY = l1xMin <= l2x1 && l1xMax >= l2x1
            -- horizontal contour
            nonIntersectH = l1x1 == l2x1 || l1x2 == l2x1 
            -- vertical contour
            nonIntersectV = l1y1 == l2y1 || l1y2 == l2y1

-- Only for comparing a horizontal with a vertical line segment
noIntersections :: Direction -> Lines -> Lines -> Bool
noIntersections _ tls []        = True
noIntersections d tls (l:rls)   | i1        = False
                                | otherwise = noIntersections d tls rls 
    where
        i1 = or [ doesIntersectHV d tl l | tl <- tls ]

maxInternalRectangle :: Locations -> Int
maxInternalRectangle []     = 0
maxInternalRectangle ls0    = fst (rls !! ix1r)
    where
        ls      = ls0 ++ [head ls0]
        hols    = getHorizontalOutlines ls
        vols    = getVerticalOutlines ls
        hvols   = zip hols vols
        rls     = allRectangles ls
        hlss    = getHorizontalLines rls
        vlss    = getVerticalLines rls
        dlss    = getDiagonalLines rls
        hvlss   = zip hlss vlss
        ixdls   = zip [0..] dlss
        ix1r    = head [ ix | ((ix,dl),(hls,vls)) <- zip ixdls hvlss, 
                                (   (noIntersections Horizontal hols vls)
                                &&  (noIntersections Vertical   vols hls)
                                &&  (noDiagonalIntersections ls dl)) ]

-- ------------------------------------------------------------------------------------------------------------
-- Helper code -- Contour plot with the help of Gloss -- 
-- Gloss code
-- Contour plot
--
scalePath :: Locations -> Path
scalePath = map scaleXY
    where
        factor  = 0.008 :: Float -- 50.0
        delta   = 50000 -- 5
        scaleXY (ix,iy) = ( factor * (fromIntegral ix - delta),
                            factor * (fromIntegral iy - delta))
 
plotSolution :: Locations -> IO ()
plotSolution points
    | points == []  = return ()
    | otherwise     = display window background $ pictures lines
        where
            mx2         = maxInternalRectangle points
            corners     = snd . head . filter ((== mx2) . fst) $ allRectangles points
            window      = InWindow "Contour plot with maximum rectangle" (800,800) (40,30)
            background  = makeColor 0.02 0.19 0.05 1.00  -- rgba dark green
            outline     = color (light green) . line . scalePath $ points  ++ [head points ]
            rectangle   = color yellow .        line . scalePath $ corners ++ [head corners] 
            lines       = if (corners == []) then [outline] else [outline] ++ [rectangle]

--
-- ------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 9  (Haskell)"
            day9 <- map parsePosition <$> lines <$> readFile filename
            putStr "Part one: The largest rectangle is: "
            print $ maxRectangle day9
            putStr "Part two: The largest rectangle is: "
            print $ maxInternalRectangle day9       
            putStrLn "0K.\n"
        -- 
        {-  ---------------------------------------------------------------------
            -- Code for an optional plot 
            plotSolution day9
        -- -}

-- End of code

