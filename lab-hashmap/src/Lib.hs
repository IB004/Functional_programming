module Lib
    ( someFunc
    ) where

import Data.Hashable (Hashable, hash)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Buckect k v = [(k, v)]
data HashMap k v = HashMap {buckets :: Vector (Buckect k v), elementsCount :: Int}
    deriving (Show)


emptyHashMap :: Int -> HashMap k v 
emptyHashMap bucketsCount = HashMap {
                                        buckets = V.fromList $ take bucketsCount $ repeat [],
                                        elementsCount = 0
                                    }


hashToIndex :: Integral a => a -> a -> a 
hashToIndex hashCode size = abs hashCode `mod` size


insert k v hashMap = HashMap {
                                buckets = buckets hashMap // [(index, newBucket)],
                                elementsCount = elementsCount hashMap + deltaLength
                            }
    where 
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    oldBucket = buckets hashMap ! index
    newBucket = (k, v) : ( filter (\(kx, _) -> kx /= k) oldBucket )
    deltaLength = length newBucket - length oldBucket
    

delete k hashMap = HashMap {
                                buckets = buckets hashMap // [(index, newBucket)],
                                elementsCount = elementsCount hashMap + deltaLength
                            }
    where 
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    oldBucket = buckets hashMap ! index
    newBucket = filter (\(kx, _) -> kx /= k) oldBucket
    deltaLength = length newBucket - length oldBucket