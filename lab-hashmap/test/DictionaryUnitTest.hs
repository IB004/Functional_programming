module DictionaryUnitTest
(
tests
)
where

import Test.Tasty
import Test.Tasty.HUnit

import Dict
import HashMap

tests :: TestTree
tests = testGroup "Unit tests" [test1, test2, test3]


test1 :: TestTree
test1 = testCaseSteps "Insert test" $ \step -> do
  step "Preparing..."
  let dict0 = emptyD :: HashMap Int Int
  let dict1 = insertD 5 55 $ insertD 2 2 $ insertD 11 101 $ insertD 0 100 dict0 

  step "Elements count check"
  elementsCountD dict1 @?= 4

  step "Lookup inserted check"
  lookupD 0 dict1 @?= Just 100
  lookupD 11 dict1 @?= Just 101
  lookupD 2 dict1 @?= Just 2
  lookupD 5 dict1 @?= Just 55

  step "Lookup not inserted check"
  lookupD 1 dict1 @?= Nothing
  lookupD 3 dict1 @?= Nothing
  lookupD 55 dict1 @?= Nothing


test2 :: TestTree
test2 = testCaseSteps "Replace test" $ \step -> do
  step "Preparing..."
  let dict0 = emptyD :: HashMap Int String
  let dict1 = insertD 2 "Bye" $ insertD 11 "Hi" $ insertD 0 "Hello" dict0 

  step "Elements count check"
  elementsCountD dict1 @?= 3

  step "Lookup inserted check"
  lookupD 0 dict1 @?= Just "Hello"
  lookupD 11 dict1 @?= Just "Hi"
  lookupD 2 dict1 @?= Just "Bye"

  step "Replace inserted.."
  let dict2 = insertD 11 "Greeting" dict1 
  let dict3 = insertD 2 "Farewell" dict2

  step "Elements count check"
  elementsCountD dict3 @?= 3

  step "Lookup replaced check"
  lookupD 0 dict3 @?= Just "Hello"
  lookupD 11 dict3 @?= Just "Greeting"
  lookupD 2 dict3 @?= Just "Farewell"


test3 :: TestTree
test3 = testCaseSteps "Delete test" $ \step -> do
  step "Preparing..."
  let dict0 = emptyD :: HashMap String Float
  let dict1 = insertD "Cheese" 0 $ insertD "Eggs" 3.0 $ insertD "Milk" 2.5 dict0 

  step "Elements count check"
  elementsCountD dict1 @?= 3

  step "Lookup inserted check"
  lookupD "Milk" dict1 @?= Just 2.5
  lookupD "Eggs" dict1 @?= Just 3.0
  lookupD "Cheese" dict1 @?= Just 0

  step "Delete elements..."
  let dict2 = deleteD "Cheese" dict1

  step "Elements count check"
  elementsCountD dict2 @?= 2

  step "Lookup check"
  lookupD "Milk" dict2 @?= Just 2.5
  lookupD "Eggs" dict2 @?= Just 3.0
  lookupD "Cheese" dict2 @?= Nothing

  step "Delete rest elements..."
  let dict3 = deleteD "Milk" $ deleteD "Eggs" dict2

  step "Elements count check"
  elementsCountD dict3 @?= 0

  step "Lookup check"
  lookupD "Milk" dict3 @?= Nothing
  lookupD "Eggs" dict3 @?= Nothing
  lookupD "Cheese" dict3 @?= Nothing
