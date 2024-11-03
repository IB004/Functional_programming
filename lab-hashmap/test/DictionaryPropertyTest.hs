{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module DictionaryPropertyTest
(
tests
)
where
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Hashable (Hashable)

import Dict
import HashMap

tests :: TestTree
tests = testGroup "Property tests" [test1]

test1 = testProperty "Mempty test" test 
	where
	test :: HashMap Int Int -> Bool
	test dict = ((dict <> mempty) `eqD` dict) && ((mempty <> dict) `eqD` dict) 
