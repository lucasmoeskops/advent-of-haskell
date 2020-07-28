{- Advent of Code year 2019 day 6 solution in Haskell 
 - Author: Lucas Moeskops
 - Date: July 28th 2020
 -
 - Typical invocation:
 -  $ cat y2019d06.in | stack runhaskell y2019d06.hs
 -}

import Data.List.Split (splitOn)

{- Object with label and orbiting children -}
data Object = Object String [Object]

instance Eq Object where
    (Object n _) == (Object m _) = m == n

-- Calculate number of total orbits in a map
calcTotalOrbits map = calcDirectOrbits map + calcIndirectOrbits map

-- Calculate number of direct orbits in a map
calcDirectOrbits (Object _ children) = ownOrbits + childrenOrbits
    where 
        ownOrbits      = length children
        childrenOrbits = sum $ map calcDirectOrbits children

-- Calculate number of indirect orbits in a map
calcIndirectOrbits (Object _ children) = childIndirectOrbits
    where
        calcIndirectOrbits' acc (Object _ children') = ownOrbits + childrenOrbits
            where
                ownOrbits      = acc
                childrenOrbits = sum $ map (calcIndirectOrbits' (acc + 1)) children'
        childIndirectOrbits                          = sum $ map (calcIndirectOrbits' 0) children

-- Calculate required orbital transfers to move between two objects
orbitalTransfersRequired object from to =
    case calcDistance object from to of
        Just d  -> Just (d - 2)
        Nothing -> Nothing

-- Calculate distance between two objects
-- Not very efficient, since results aren't stored inbetween, and are recalculated as long
-- as they are in the same sub-branch while descending in the map
-- Still efficient enough for the given map.
calcDistance root@(Object a children) from to = findDistance
    where
        calcDistance' target (Object label children') =
            if label == target then
                Just 0
            else
                foldr (getMatch' . calcDistance' target) Nothing children'
            where
                getMatch' (Just d) _        = Just (1 + d)
                getMatch' _        (Just d) = Just d
                getMatch' _        _        = Nothing
        searchObject childObject                   = 
            case calcDistance' from childObject of
                Just d ->
                    case calcDistance' to childObject of
                        Just e  -> calcDistance childObject from to
                        Nothing ->
                            case calcDistance' to root of
                                Just f  -> Just (1 + d + f)
                                Nothing -> Nothing
                Nothing -> Nothing
        getMatch (Just d) _                        = Just d
        getMatch _        (Just d)                 = Just d
        getMatch _        _                        = Nothing
        findDistance                               = foldr (getMatch . searchObject) Nothing children


-- Build a map from a string and the label of the first element
buildFromString string = buildObject
    where
        makeEntry           = \[x,y] -> (x,y)
        isValid             = (==2) . length
        entries             = map makeEntry $ filter isValid $ map (splitOn ")") $ splitOn "\n" string
        findChildren label' = map snd $ filter ((==label') . fst) entries
        buildObject label'  = Object label' $ map buildObject $ findChildren label'

main =
    do
        contents <- getContents
        let
            map      = buildFromString contents "COM"
            orbits   = show $ calcTotalOrbits map
            distance = maybe "unknown" show (orbitalTransfersRequired map "YOU" "SAN")
        print $ "The total number of direct and indirect orbits in the map data is " ++ orbits ++ "."
        print $ "The distance to Santa is " ++ distance ++ "."
