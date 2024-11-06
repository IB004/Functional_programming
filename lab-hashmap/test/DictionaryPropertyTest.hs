{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module DictionaryPropertyTest
(
tests
)
where
import Test.Tasty
import Test.Tasty.QuickCheck

import Dict
import HashMap

tests :: TestTree
tests = testGroup "Property tests" [test1, test2, test3, test4, test5, test6]


test1 :: TestTree
test1 = testProperty "Mempty test" test 
    where
    test :: HashMap Int Int -> Bool
    test dict = ((dict <> mempty) `eqD` dict) && ((mempty <> dict) `eqD` dict) 


test2 :: TestTree
test2 = testProperty "Concat associativity test" test 
    where
    test :: HashMap String Bool -> HashMap String Bool -> HashMap String Bool -> Bool
    test dict1 dict2 dict3 = ((dict1 <> dict2) <> dict3) `eqD` (dict1 <> (dict2 <> dict3)) 


test3 :: TestTree
test3 = testProperty "Foldr to list and restore back" test 
    where
    test :: HashMap Int Int ->  Bool
    test dict = restored `eqD` dict
        where
        lst = reverse $ foldrD (:) [] dict 
        restored = foldr (uncurry insertD) emptyD lst 


test4 :: TestTree
test4 = testProperty "Foldl to list length is elemetsCount" test 
    where
    test :: HashMap Int Int ->  Bool
    test dict = length (foldlD (flip (:)) [] dict) == elementsCountD dict


test5 :: TestTree
test5 = testProperty "Filter doesn't increase elemetsCount" test 
    where
    test :: HashMap Int Int -> Bool
    test dict = elementsCountD (filterD predic dict) <= elementsCountD dict
        where
        predic (a, b) = a < 25 && b > 16


test6 :: TestTree
test6 = testProperty "Map doesn't change elemetsCount" test 
    where
    test :: HashMap Int Int -> Bool
    test dict = elementsCountD (mapD (\(a, _) -> (a, a)) dict) == elementsCountD dict
