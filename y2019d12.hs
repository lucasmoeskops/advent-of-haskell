{- Advent of Code year 2019 day 12 solution in Haskell
 - Title: The N-Body Problem
 - Author: Lucas Moeskops
 - Date: July 30th 2020
 -
 - Typical invocation:
 -  $ cat y2019d12.in | stack runhaskell y2019d12.hs
 -}

import Data.List (transpose)
import Data.List.Split (splitOn)

type Vector   = [Int]
type Position = Vector
type Velocity = Vector

-- A moon with position and velocity
data Moon = Moon Position Velocity

-- Moons can be compared
instance Eq Moon where
    (Moon ps vs) == (Moon ps' vs') = ps == ps' && vs == vs'

-- A system is a collection of moons
type System = [Moon]

-- A list of states over time for a given system
evolution system = system : [system' | system' <- map applyTimeStep (evolution system)]

-- Calculate the next state for a system
applyTimeStep system = map applyVelocity $ map applyGravity system
    where
        sign n | n > 0               = 1
               | n < 0               = -1
               | n == 0              = 0
        change value                 = foldr ((+) . sign . subtract value) 0
        applyGravity (Moon ps vs)  = (Moon ps newVelocity)
            where
                positions   = map (\(Moon pos _) -> pos) system
                newVelocity = [v + (change p cs) | (p, v, cs) <- zip3 ps vs (transpose positions)]
        applyVelocity (Moon ps vs) = (Moon newPosition vs)
            where
                newPosition = [p + v | (p, v) <- zip ps vs]

-- Calculate the total energy in a system
calcTotalEnergy :: System -> Int
calcTotalEnergy = foldr ((+) . (\moon -> potentialEnergy moon * kineticEnergy moon)) 0
    where
        energy                        = foldr ((+) . abs) 0
        potentialEnergy (Moon ps _ ) = energy ps
        kineticEnergy   (Moon _ vs)  = energy vs

-- Calculate unique states of the system
-- do so by solving per independent variable and finding the least common multiple of
-- these results.
-- * Assume length is on arriving at start state
-- * An optimisation seems to be to only compare the velocity and double the result if
--   it is equal to the original velocity, if it was 0. This makes some
--   sense but is not added, because I can't prove it yet.
calcUniqueStates :: System -> Int
calcUniqueStates system = foldr (lcm . solveForDimension) 1 [0..numDimensions - 1]
    where
        numDimensions     = let (Moon ps _) = head system in length ps
        solveForDimension i = search initialState
            where
                initialState = map (\(Moon ps vs) -> Moon [ps !! i] [vs !! i]) system
                search state = search' state 1
                    where
                        search' state acc =
                            let
                                nextState = applyTimeStep state
                            in
                                if nextState == initialState then
                                    acc
                                else
                                    search' nextState (acc + 1)

-- Build a system from a string
buildFromString string = map compileMoon $ splitOn "\n" string
    where
        isValidChar c = c == ',' || c == '-' || c >= '0' && c <= '9'
        compileMoon rawMoon = Moon ps (replicate (length ps) 0)
            where
                ps = map read $ splitOn "," $ filter isValidChar rawMoon

main =
    do
        contents <- getContents
        let
            system       = buildFromString contents
            system'      = evolution system !! 1000
            totalEnergy  = show $ calcTotalEnergy system'
            numSteps     = show $ calcUniqueStates system
        print $ "The total energy in the system after 1000 runs is " ++ totalEnergy ++ "."
        print $ "The amount of steps in the system is " ++ numSteps ++ "."
