{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Test.QuickCheck (Arbitrary, arbitrary, elements, listOf1, Gen)

import Control.Concurrent.STM (newTVarIO, readTVarIO, TVar)
import Control.Concurrent (newChan, Chan)
import Control.Monad.IO.Class (liftIO)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests]

instance Arbitrary Lib2.Name where
    arbitrary = do
        n <- choose (1, 100) :: Gen Int
        s <- listOf1 $ elements ['a'..'z']
        elements [Lib2.StringName s]

instance Arbitrary Lib2.Query where
    arbitrary = do
        name <- arbitrary
        route <- arbitrary
        stop <- arbitrary
        oneof [ pure $ Lib2.ListCreate name
              , pure $ Lib2.ListAdd name route
              , pure $ Lib2.ListGet name
              , pure $ Lib2.ListRemove name
              , pure $ Lib2.RouteCreate name
              , pure $ Lib2.RouteGet name
              , pure $ Lib2.RouteAddRoute route route
              , pure $ Lib2.RouteAddStop name stop
              , pure $ Lib2.RouteRemoveStop name stop
              , pure $ Lib2.RouteRemove name
              , pure $ Lib2.StopCreate name
              , pure $ Lib2.StopDelete name
              , pure $ Lib2.RoutesFromStop stop
              ]

instance Arbitrary Lib2.State where
    arbitrary = do
        routeTreeLists <- resize 5 arbitrary
        routes <- resize 5 arbitrary
        stops <- resize 5 arbitrary
        pure $ Lib2.State routeTreeLists routes stops

instance Arbitrary Lib2.Route where
    arbitrary = do
        routeId' <- arbitrary
        stops' <- resize 5 arbitrary
        nestedRoutes' <- resize 5 arbitrary
        pure $ Lib2.Route routeId' stops' nestedRoutes'

instance Arbitrary Lib2.Stop where
    arbitrary = do
        stopId' <- arbitrary
        pure $ Lib2.Stop stopId'

instance Arbitrary Lib2.RouteTree where
    arbitrary = do
        nodeRoute <- arbitrary
        routeTrees <- resize 5 arbitrary
        oneof [ pure Lib2.EmptyTree
              , pure $ Lib2.Node nodeRoute routeTrees
              ]

instance Arbitrary Lib2.NodeRoute where
    arbitrary = do
        nodeRouteId <- arbitrary
        nodeStops <- resize 5 arbitrary
        pure $ Lib2.NodeRoute nodeRouteId nodeStops

-- Lib3 statements
instance Arbitrary Lib3.Statements where
 arbitrary = frequency
  [ (3, Lib3.Single <$> arbitrary) 
  , (1, Lib3.Batch <$> resize 5 (listOf1 (arbitrary @Lib2.Query)))
  ]


propertyTests :: TestTree
propertyTests =
    testGroup "Property tests"
    [ QC.testProperty "parseStatements . renderStatements == Right query" $  -- Test for statement integrity
    \query -> 
        let 
        rendered = Lib3.renderStatements query 
        parsed = Lib3.parseStatements rendered 
        in 
        counterexample ("Original query: " ++ show query ++ 
                        "\nRendered: " ++ rendered ++ 
                        "\nParsed result: " ++ show parsed) $ 
            case parsed of 
            Right (parsedQuery, "") -> query == parsedQuery 
            _ -> False
        , QC.testProperty "Parsing invalid statements returns an error" $
    \invalidInput ->
        case Lib3.parseStatements invalidInput of
        Left _ -> True  -- Should return an error for invalid input
        _ -> False

    , QC.testProperty "Empty batch statements can be parsed" $
    Lib3.parseStatements (Lib3.renderStatements (Lib3.Batch [])) 
        === Right (Lib3.Batch [], "")

    , QC.testProperty "Large batch statements maintain integrity" $
    \largeBatch ->
        let 
        rendered = Lib3.renderStatements (Lib3.Batch largeBatch)
        parsed = Lib3.parseStatements rendered
        in
        case parsed of
            Right (Lib3.Batch parsedBatch, "") -> 
                length parsedBatch == length largeBatch
            _ -> False

    ]