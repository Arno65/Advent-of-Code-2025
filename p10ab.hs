-- Advent of Code 2025 - Day 10 part One and Two
-- Solutions in Haskell
-- https://adventofcode.com/2025/day/10
-- (Ter leering ende vermaeck...)
--
--
--  Part one: The fewest button presses required to correctly configure
--            the indicator lights on all of the machines is:            535
--  Part two: The fewest button presses required to correctly configure
--            the joltage level counters on all of the machines is:      21021
--
--  This time with the help of Z3, a sat-solver
--
-- (cl) by Arno Jacobs, 2025-12-17

-- 
module AoC2025d10ab where

import Data.Char        (isDigit)
import Data.List        (subsequences)
import Data.List.Split  (splitOn)
import System.Process   (proc,readCreateProcess,std_in,StdStream(CreatePipe))

data Light = Off | On deriving Eq

instance Show Light where
    show Off = "."
    show On  = "#"

type Lights     = [Light]
type Switch     = Int
type SwitchSets = [Int]
type Switches   = [SwitchSets]
type Joltage    = [Int]
type Machine    = (Lights,Switches,Joltage)
type Machines   = [Machine]    
type Steps      = [[Int]]

filename :: String
filename = "data/inputDay10_2025.txt"

------------------------------------------------------------------------------------------------
-- Parse code
--
parseMachine :: String -> Machine
parseMachine ml = (getLights ml, getSwitches ml, getJoltageRequirements ml)

getLights :: String -> Lights
getLights = map toLight . takeWhile (/=']') . tail . dropWhile (/='[')
    where
        toLight '.' = Off
        toLight  _  = On

getSwitches :: String -> Switches
getSwitches = map toSwitches . splitOn " " . init . takeWhile (/='{') . dropWhile (/='(')
    where
        toSwitches = map read . splitOn "," . takeWhile (/=')') . tail . dropWhile (/='(')

getJoltageRequirements :: String -> Joltage
getJoltageRequirements = map read . splitOn "," . takeWhile (/='}') . tail . dropWhile (/='{')

------------------------------------------------------------------------------------------------
-- Helper code
--
extractLights :: Machines -> [Lights] 
extractLights   = map (\(l,_,_) -> l)

extractSwitches :: Machines -> [Switches]
extractSwitches = map (\(_,s,_) -> s)

extractJoltage :: Machines -> [Joltage]
extractJoltage = map (\(_,_,j) -> j)

allLightsOff :: Lights -> Bool
allLightsOff = and . map (==Off)

------------------------------------------------------------------------------------------------
-- Part one
--
partOne :: Machines -> Int
partOne machines = sum $ map workSwitches machinesOne
    where
        lights      = extractLights machines
        switches    = extractSwitches machines
        machinesOne = zip lights switches

workSwitches :: (Lights,Switches) -> Int
workSwitches (lights,switches) = minimum $ map fst allSwitchedOff
    where
        --                          fn  'subsequences' is 'combinations'
        allSwitchCombinations   = tail $ subsequences switches    
        countButtonPresses      = map length allSwitchCombinations 
        allLightCombinations    = workAllSwitchCombinations lights allSwitchCombinations
        allCounted              = zip countButtonPresses allLightCombinations
        allSwitchedOff          = filter (allLightsOff . snd) allCounted 

workAllSwitchCombinations :: Lights -> [Switches] -> [Lights]        
workAllSwitchCombinations lights = map (workSwitchesCombination lights) . map concat
    where
        workSwitchesCombination lights []               = lights
        workSwitchesCombination lights (switch:rsws)    = 
            workSwitchesCombination (toggleLights lights switch) rsws
        --
        toggleLights lights switch = firstLights ++ [toggleLight] ++ restLights
            where
                firstLights = take  switch    lights
                restLights  = drop (switch+1) lights
                toggleLight = if (lights !! switch) == Off then On else Off

------------------------------------------------------------------------------------------------
-- Part two
--
partTwo :: Machines -> IO Int
partTwo machines = 
    do  z3outputs <- mapM (readCreateProcess 
                            (proc "z3" ["-in"])         -- There is no sat-checking
                            { std_in = CreatePipe })    -- Assuming every logic quest can be solved
                                (generateSMTs machines)
        return $ sum $ map getZ3Result z3outputs

-- Call in the troops - the Z3 sat-solver
-- 
generateSMTs :: Machines -> [String]
generateSMTs machines = map (\(j,sw) -> generateSMT j sw) $ zip joltages switches
    where
        joltages = extractJoltage machines
        switches = extractSwitches machines

generateSMT :: Joltage -> Switches -> String
generateSMT joltage switches = 
    unlines
        [ "(set-logic LIA)", "(set-option :produce-models true)"
        , declares, nonNegatives, counterConstraints, objective
        , "(check-sat)", "(get-objectives)", "(exit)" ]
            where
                numSwitches     = length switches
                numCounters     = length joltage
                declares        = unlines ["(declare-const s" ++ show i ++ " Int)" | i <- [0..numSwitches-1]]
                nonNegatives    = unlines ["(assert (>= s" ++ show i ++ " 0))" | i <- [0..numSwitches-1]]
                counterConstraints = unlines
                    [ "(assert (= " ++ buildSum counterIdx ++ " " ++ show (joltage !! counterIdx) ++ "))"
                    | counterIdx <- [0..numCounters-1]]        
                buildSum counterIdx | affectingSwitches == []   = "0"
                                    | otherwise                 = bs
                    where
                        affectingSwitches = [ j | (j, sw) <- zip [0..] switches, elem counterIdx sw]
                        bs = if length affectingSwitches == 1           then 
                                "s"   ++ show (head affectingSwitches)  else 
                                "(+ " ++ unwords ["s" ++ show j | j <- affectingSwitches] ++ ")"
                objective = "(minimize (+ " ++ unwords ["s" ++ show i | i <- [0..numSwitches-1]] ++ "))"        
            
-- Get the last Int from a given String
getZ3Result :: String -> Int
getZ3Result z3r 
    | noDigits  = 0
    | otherwise = (read . reverse . takeWhile (isDigit) . dropWhile (not . isDigit) . reverse) z3r
        where
            noDigits = [] == filter isDigit z3r

main :: IO ()
main = do   putStrLn "Advent of Code 2025 - day 10  (Haskell)"
            day10 <- map parseMachine <$> lines <$> readFile filename
            putStrLn "Part one: The fewest button presses required to correctly configure"
            putStr   "          the indicator lights on all of the machines is:            "
            print $ partOne day10
            --
            putStrLn "Part two: The fewest button presses required to correctly configure"
            putStr   "          the joltage level counters on all of the machines is:      "
            p2 <- partTwo day10
            print p2
            --
            putStrLn "0K.\n"


{-

An example of the 'piped' input for the Z3 sat-solver based on:
switches: [[0,1,5,6,7,8,9],[4,5],[1,2,3,5,6],[0,3],[8,9],[0,3,5,6,7,8,9],[0,1,4,6,7,9],[1,2],[5,8]]
joltages: [51,38,12,25,9,52,42,42,58,49]

((set-logic LIA)
(set-option :produce-models true)
(declare-const s0 Int)
(declare-const s1 Int)
(declare-const s2 Int)
(declare-const s3 Int)
(declare-const s4 Int)
(declare-const s5 Int)
(declare-const s6 Int)
(declare-const s7 Int)
(declare-const s8 Int)

(assert (>= s0 0))
(assert (>= s1 0))
(assert (>= s2 0))
(assert (>= s3 0))
(assert (>= s4 0))
(assert (>= s5 0))
(assert (>= s6 0))
(assert (>= s7 0))
(assert (>= s8 0))

(assert (= (+ s0 s3 s5 s6) 51))
(assert (= (+ s0 s2 s6 s7) 38))
(assert (= (+ s2 s7) 12))
(assert (= (+ s2 s3 s5) 25))
(assert (= (+ s1 s6) 9))
(assert (= (+ s0 s1 s2 s5 s8) 52))
(assert (= (+ s0 s2 s5 s6) 42))
(assert (= (+ s0 s5 s6) 42))
(assert (= (+ s0 s4 s5 s8) 58))
(assert (= (+ s0 s4 s5 s6) 49))

(minimize (+ s0 s1 s2 s3 s4 s5 s6 s7 s8))
(check-sat)
(get-objectives)
(exit)

-}

-- End of code
