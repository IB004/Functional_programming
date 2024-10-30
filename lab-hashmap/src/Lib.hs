module Lib
    ( someFunc ) 
    where

import Data.Hashable (Hashable, hash)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.List (foldl')

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type Bucket k v = [(k, v)]

data HashMap k v = HashMap {buckets :: Vector (Bucket k v), elementsCount :: Int}
    deriving (Show)


emptyHashMap :: Int -> HashMap k v 
emptyHashMap bucketsCount = HashMap {
                                        buckets = V.replicate bucketsCount [],
                                        elementsCount = 0
                                    }


clearHashMap hashMap = emptyHashMap $ V.length $ buckets hashMap



hashToIndex :: Integral a => a -> a -> a 
hashToIndex hashCode size = abs hashCode `mod` size


toListH :: HashMap k v -> [(k, v)]
toListH hashMap = concat $ V.toList $ buckets hashMap


fromListH :: Hashable k => Int -> [(k, v)] -> HashMap k v
fromListH bucketsCount lst = fromListRec lst (emptyHashMap bucketsCount)
    where
    fromListRec [] hashMap = hashMap
    fromListRec ( (k, v) : xs ) hashMap = fromListRec xs (insertH k v hashMap)


countBucketsElements :: Vector (Bucket k v) -> Int
countBucketsElements buckets | V.null buckets = 0
                             | otherwise = (length $ V.head buckets) + (countBucketsElements $ V.tail buckets)


lookupH :: (Hashable k, Eq k) => k -> HashMap k v -> Maybe v 
lookupH k hashMap =  if null res then Nothing else Just (snd $ head $ res)
    where
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    bucket = buckets hashMap ! index
    res = filter (\(k1, _) -> k1 == k) bucket 


insertH :: Hashable k => k -> v -> HashMap k v -> HashMap k v
insertH k v hashMap = HashMap {
                                buckets = buckets hashMap // [(index, newBucket)],
                                elementsCount = elementsCount hashMap + deltaLength
                            }
    where 
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    oldBucket = buckets hashMap ! index
    newBucket = (k, v) : ( filter (\(kx, _) -> kx /= k) oldBucket )
    deltaLength = length newBucket - length oldBucket


concatH hm1 hm2 bucketsCount = insertAllRec (toListH hm1) (toListH hm2) (emptyHashMap bucketsCount)
    where
    insertAllRec [] [] hashMap = hashMap
    insertAllRec ((k, v) : xs) hm2 hashMap = insertAllRec xs hm2 (insertH k v hashMap)
    insertAllRec [] ((k, v) : xs) hashMap = insertAllRec [] xs (insertH k v hashMap)



deleteH :: Hashable k => k -> HashMap k v -> HashMap k v
deleteH k hashMap = HashMap {
                                buckets = buckets hashMap // [(index, newBucket)],
                                elementsCount = elementsCount hashMap + deltaLength
                            }
    where 
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    oldBucket = buckets hashMap ! index
    newBucket = filter (\(kx, _) -> kx /= k) oldBucket
    deltaLength = length newBucket - length oldBucket


filterH :: (v -> Bool) -> HashMap k v -> HashMap k v
filterH predic hashMap = HashMap {
                                    buckets = newBuckets,
                                    elementsCount = countBucketsElements newBuckets
                                 }
    where    
    newBuckets = filterBuckets predic $ buckets hashMap

    filterOneBucket :: (v -> Bool) -> Bucket k v -> Bucket k v
    filterOneBucket predic bucket = filter (\(_, v) -> predic v) bucket
    
    filterBuckets :: (v -> Bool) -> Vector (Bucket k v) -> Vector (Bucket k v)
    filterBuckets predic buckets | V.null buckets = V.empty 
                                 | otherwise = filterOneBucket predic (V.head buckets) `V.cons` filterBuckets predic (V.tail buckets)


mapH :: (v -> w) -> HashMap k v -> HashMap k w
mapH func hashMap = HashMap {
                                buckets = newBuckets,
                                elementsCount = elementsCount hashMap
                            }
    where
    newBuckets = mapBuckets func $ buckets hashMap

    mapOneBucket :: (v -> w) -> Bucket k v -> Bucket k w
    mapOneBucket func bucket = map (\(k, v) -> (k, func v)) bucket
    
    mapBuckets :: (v -> w) -> Vector (Bucket k v) -> Vector (Bucket k w)
    mapBuckets func buckets | V.null buckets = V.empty 
                            | otherwise = mapOneBucket func (V.head buckets) `V.cons` mapBuckets func (V.tail buckets)


foldrH :: ((k, v) -> b -> b) -> b -> HashMap k v -> b
foldrH func ini hashMap = foldr func ini $ toListH hashMap


foldlH :: (b -> (k, v) -> b) -> b -> HashMap k v -> b
foldlH func ini hashMap = foldl' func ini $ toListH hashMap


mapF :: Hashable k2 => ((k1, v1) -> (k2, v2)) -> HashMap k1 v1 -> HashMap k2 v2
mapF func hashMap = foldrH (\el hm -> insertH (fst $ func el) (snd $ func el) hm) (emptyHashMap $ V.length $ buckets hashMap) hashMap


filterF :: Hashable k => ((k, v) -> Bool) -> HashMap k v -> HashMap k v
filterF predic hashMap = foldrH (\el hm -> if predic el then insertH (fst el) (snd el) hm else hm) (clearHashMap hashMap) hashMap


example :: HashMap Int String 
example = insertH 3 "D" $ insertH 2 "C" $ insertH 11 "B" $ insertH 1 "A" $ emptyHashMap 10   

example2 :: HashMap Int Int 
example2 = insertH 3 333 $ insertH 2 22 $ insertH 11 1 $ insertH 1 111 $ emptyHashMap 10   