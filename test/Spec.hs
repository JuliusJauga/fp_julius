{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified (parseCommand, Query(..), Route(..), Stop(..), State(..), RouetTree, stateTransition, initialState)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests" [
    -- Parsing Tests
    
    
    ]