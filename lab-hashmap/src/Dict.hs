module Dict 
(
    Dictionary
)
where

import Data.Hashable (Hashable)

class Dictionary dict where
    insertD :: (Hashable k) => k -> v -> dict k v -> dict k v
