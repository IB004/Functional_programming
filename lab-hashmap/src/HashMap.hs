module HashMap 
(
    Bucket, 
    HashMap,
    emptyHashMap,
    clearHashMap,
    toList,
    fromList,
    countElements,
    countBuckets,
    lookupH,
    insertH,
    concatH,
    deleteH,
    filterV,
    mapV,
    foldrH,
    foldlH,
    mapH,
    filterH,
    example,
    example2,

    someFunc,
)
where

import Data.Hashable (Hashable, hash)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), choose, listOf)

import Dict

someFunc :: IO ()
someFunc = putStrLn "someFunc"




------- HashMap type -------

type Bucket k v = [(k, v)]


data HashMap k v = HashMap {buckets :: Vector (Bucket k v), elementsCount :: Int}
    deriving (Show)


instance (Hashable k) => Semigroup (HashMap k v) where
    (<>) hm1 hm2 = concatH (max (countBuckets hm1) (countBuckets hm2)) hm1 hm2 


instance (Hashable k) => Monoid (HashMap k v) where
    mempty = emptyHashMap 11


instance Dictionary HashMap where
    emptyD = emptyHashMap 11
    elementsCountD = elementsCount

    lookupD = lookupH
    insertD = insertH
    deleteD = deleteH

    foldrD = foldrH
    foldlD = foldlH
    mapD = mapH
    filterD = filterH

{-
instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
    arbitrary = do 
        n <- choose (1, 50 :: Int)
        bCount <- choose (1, 23)
        return HashMap { buckets = V.replicate bCount [], elementsCount = 0 }
        -- hashMap <- iterate (insertH arbitrary arbitrary) empty !! n
-}

instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = do
    bCount <- choose (1, 23)
    pairs <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ fromList bCount pairs 
        





------- Util functions  -------

emptyHashMap :: Int -> HashMap k v 
emptyHashMap bucketsCount = HashMap {
                                        buckets = V.replicate bucketsCount [],
                                        elementsCount = 0
                                    }


clearHashMap :: HashMap k v -> HashMap k v 
clearHashMap hashMap = emptyHashMap $ countBuckets hashMap


hashToIndex :: (Integral a) => a -> a -> a 
hashToIndex hashCode size = abs hashCode `mod` size


toList :: HashMap k v -> [(k, v)]
toList hashMap = concat $ V.toList $ buckets hashMap


fromList :: (Hashable k) => Int -> [(k, v)] -> HashMap k v 
fromList bucketsCount lst = fromListRec lst (emptyHashMap bucketsCount)
    where
    fromListRec [] hashMap = hashMap
    fromListRec ( (k, v) : xs ) hashMap = fromListRec xs (insertH k v hashMap)


countElements :: Vector (Bucket k v) -> Int
countElements bucketsVector = length $ concat $ V.toList bucketsVector


countBuckets :: HashMap k v -> Int 
countBuckets hashMap = length $ buckets hashMap




------- Basic functions  -------

lookupH :: (Hashable k) => k -> HashMap k v -> Maybe v 
lookupH k hashMap =  if null res then Nothing else Just (snd $ head res)
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
    newBucket = (k, v) : filter (\(kx, _) -> kx /= k) oldBucket
    deltaLength = length newBucket - length oldBucket


concatH :: Hashable k => Int -> HashMap k v -> HashMap k v -> HashMap k v 
concatH bucketsCount hm1 hm2  = fromList bucketsCount (toList hm1 ++ toList hm2)


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


filterV :: (v -> Bool) -> HashMap k v -> HashMap k v
filterV predic hashMap = HashMap {
                                    buckets = newBuckets,
                                    elementsCount = countElements newBuckets
                                 }
    where    
    newBuckets = filterBuckets $ buckets hashMap

    filterOneBucket = filter (predic . snd)
    
    filterBuckets bucketsVector | V.null bucketsVector = V.empty 
                                | otherwise = filterOneBucket (V.head bucketsVector) `V.cons` filterBuckets (V.tail bucketsVector)


mapV :: (v -> w) -> HashMap k v -> HashMap k w
mapV func hashMap = HashMap {
                                buckets = newBuckets,
                                elementsCount = elementsCount hashMap
                            }
    where
    newBuckets = mapBuckets $ buckets hashMap

    mapOneBucket = map (\(k, v) -> (k, func v))
    
    mapBuckets bucketsVector | V.null bucketsVector = V.empty 
                             | otherwise = mapOneBucket (V.head bucketsVector) `V.cons` mapBuckets (V.tail bucketsVector)


foldrH :: ((k, v) -> b -> b) -> b -> HashMap k v -> b
foldrH func ini hashMap = foldr func ini $ toList hashMap


foldlH :: (b -> (k, v) -> b) -> b -> HashMap k v -> b
foldlH func ini hashMap = foldl' func ini $ toList hashMap


mapH :: Hashable k2 => ((k1, v1) -> (k2, v2)) -> HashMap k1 v1 -> HashMap k2 v2
mapH func hashMap = foldrH (uncurry insertH . func) (emptyHashMap $ V.length $ buckets hashMap) hashMap


filterH :: Hashable k => ((k, v) -> Bool) -> HashMap k v -> HashMap k v
filterH predic hashMap = foldrH (\el hm -> if predic el then uncurry insertH el hm else hm) (clearHashMap hashMap) hashMap




------- Examples  -------

example :: HashMap Int String 
example = insertH 3 "D" $ insertH 2 "C" $ insertH 11 "B" $ insertH 1 "A" $ emptyHashMap 10   

example2 :: HashMap Int Int 
example2 = insertH 3 333 $ insertH 2 22 $ insertH 11 1 $ insertH 1 111 $ emptyHashMap 10   
