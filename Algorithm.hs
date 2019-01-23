module Algorithm (
    isSetCode,
    isListCode
) where

import qualified Set

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf prefix word =
    isPartOf prefix prefixList
    where
        prefixList = scanl (\acc x -> acc ++ [x]) [] word
        isPartOf _ [] = False
        isPartOf x (y:ys) = x == y || isPartOf x ys

deletePrefixIn :: (Eq a) => [a] -> [a] -> [a]
deletePrefixIn _ [] = []
deletePrefixIn [] word = word
deletePrefixIn (x:xs) (y:ys) =
    if x == y
        then deletePrefixIn xs ys
    else []

-- A^-1 B
findCuttedSufixesIn :: (Ord a) => Set.Set [a] -> Set.Set [a] -> Set.Set [a]
findCuttedSufixesIn setL setR
    | Set.null setL = Set.empty
    | otherwise = Set.union (findCuttedSufixesIn rest setR) (Set.map (deletePrefixIn x) (Set.filter (isPrefixOf x) setR))
    where
        x = Set.findMax setL
        rest = Set.deleteMax setL

calculateNSet :: (Ord a) => Set.Set [a] -> Set.Set [a] -> Set.Set [a]
calculateNSet set setPrev =
    Set.union (findCuttedSufixesIn set setPrev) (findCuttedSufixesIn setPrev set)

isSetCode :: (Ord a) => Set.Set [a] -> Bool
isSetCode set = check first setOfFirst
    where
        first = Set.delete [] (calculateNSet set set)
        setOfFirst = Set.insert first Set.empty
        check setI previousSets
            | Set.member [] setI = False
            | Set.null setIPlus = True
            | Set.member setIPlus previousSets = True
            | otherwise = check setIPlus (Set.insert setIPlus previousSets)
            where
                setIPlus = calculateNSet set setI

isListCode :: (Ord a) => [[a]] -> Bool
isListCode list =  (not areDuplicatesIn) && isSetCode (Set.fromList list)
    where
        areDuplicatesIn = Set.size (Set.fromList list) == length list
