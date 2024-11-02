import Test.Tasty
import DictionaryUnitTest


main :: IO ()
main = do
    defaultMain (testGroup "Dictionary Tests" [DictionaryUnitTest.tests])