module Dict 
(
    Dictionary,
    emptyD,
    elementsCountD,
    lookupD,
    insertD,
    deleteD,
    foldrD,
    foldlD,
    mapD,
    filterD,
    containsD,
    eqD,
)
where

import Data.Hashable (Hashable)

class Dictionary dict where
    emptyD :: dict k v
    elementsCountD :: dict k v -> Int

    lookupD :: (Hashable k) => k -> dict k v -> Maybe v
    insertD :: (Hashable k) => k -> v -> dict k v -> dict k v
    deleteD :: (Hashable k) => k -> dict k v -> dict k v

    foldrD :: ((k, v) -> x -> x) -> x -> dict k v -> x
    foldlD :: (x -> (k, v) -> x) -> x -> dict k v -> x
    mapD :: (Hashable k2) => ((k1, v1) -> (k2, v2)) -> dict k1 v1 -> dict k2 v2
    filterD :: (Hashable k) => ((k, v) -> Bool) -> dict k v -> dict k v

    containsD :: (Hashable k, Eq v) => k -> v -> dict k v -> Bool
    containsD k v dict = case lookupD k dict of 
        Just value -> v == value
        Nothing -> False

    eqD :: (Hashable k, Eq v) => dict k v -> dict k v -> Bool
    eqD dict1 dict2 = dict1Count == dict2Count && filtered1Count == dict2Count
        where
        dict1Count = elementsCountD dict1
        dict2Count = elementsCountD dict2
        filtered1 = filterD (\(k, v) -> containsD k v dict2) dict1 
        filtered1Count = elementsCountD filtered1

