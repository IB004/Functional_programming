import Test.Tasty
import DictionaryUnitTest
import DictionaryPropertyTest

main :: IO ()
main = do
    defaultMain (testGroup "Dictionary Tests" [DictionaryUnitTest.tests, DictionaryPropertyTest.tests])