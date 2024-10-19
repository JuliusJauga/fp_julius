{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified (parseCommand, Command(..), Route(..), Stop(..), State(..), stateTransition)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests" [
    -- Parsing Tests
    testCase "Parsing empty input" $
        Lib2.parseCommand "" @?= Left "Invalid command.",

    testCase "Parsing invalid command" $
        Lib2.parseCommand "invalid" @?= Left "Invalid command.",

    testCase "Parsing valid list-create command" $
        Lib2.parseCommand "list-create MyList" @?= Right (Lib2.ListCreate "MyList"),

    testCase "Parsing valid list-add command" $
        Lib2.parseCommand "list-add MyList <Route1{}>" @?= Right (Lib2.ListAdd "MyList" (Lib2.Route "Route1" [] [])),

    testCase "Parsing valid list-get command" $
        Lib2.parseCommand "list-get MyList" @?= Right (Lib2.ListGet "MyList"),

    testCase "Parsing valid list-remove command" $
        Lib2.parseCommand "list-remove MyList <Route1{}>" @?= Right (Lib2.ListRemove "MyList" (Lib2.Route "Route1" [] [])),

    testCase "Parsing valid route-create command" $
        Lib2.parseCommand "route-create Route1" @?= Right (Lib2.RouteCreate "Route1"),

    testCase "Parsing valid route-get command" $
        Lib2.parseCommand "route-get Route1" @?= Right (Lib2.RouteGet "Route1"),

    testCase "Parsing valid route-add-route command" $
        Lib2.parseCommand "route-add-route <Route1{}> <Route2{}>" @?= Right (Lib2.RouteAddRoute (Lib2.Route "Route1" [] []) (Lib2.Route "Route2" [] [])),

    testCase "Parsing valid route-remove command" $
        Lib2.parseCommand "route-remove Route1" @?= Right (Lib2.RouteRemove "Route1"),

    testCase "Parsing valid stop-create command" $
        Lib2.parseCommand "stop-create Stop1" @?= Right (Lib2.StopCreate "Stop1"),

    testCase "Parsing valid stop-delete command" $
        Lib2.parseCommand "stop-delete Stop1" @?= Right (Lib2.StopDelete "Stop1"),
    
    testCase "ListCreate command" $
        let initialState = Lib2.State []
            command = Lib2.ListCreate "list1"
            expectedState = Lib2.State [Lib2.Route "list1" [] []]
        in Lib2.stateTransition initialState command @?= Right (["Created list"], expectedState),

    testCase "ListAdd command" $
        let initialState = Lib2.State [Lib2.Route "list1" [] []]
            route = Lib2.Route "route1" [] []
            command = Lib2.ListAdd "list1" route
            expectedState = Lib2.State [Lib2.Route "list1" [] [route]]
        in Lib2.stateTransition initialState command @?= Right (["Added route to list"], expectedState),

    testCase "ListGet command" $
        let initialState = Lib2.State [Lib2.Route "list1" [] [Lib2.Route "route1" [] []]]
            command = Lib2.ListGet "list1"
        in Lib2.stateTransition initialState command @?= Right (["route1"], initialState),

    testCase "ListRemove command" $
        let initialState = Lib2.State [Lib2.Route "list1" [] [Lib2.Route "route1" [] []]]
            route = Lib2.Route "route1" [] []
            command = Lib2.ListRemove "list1" route
            expectedState = Lib2.State [Lib2.Route "list1" [] []]
        in Lib2.stateTransition initialState command @?= Right (["Removed route from list"], expectedState),

    testCase "RouteCreate command" $
        let initialState = Lib2.State []
            command = Lib2.RouteCreate "route1"
            expectedState = Lib2.State [Lib2.Route "route1" [] []]
        in Lib2.stateTransition initialState command @?= Right (["Created route"], expectedState),

    testCase "RouteGet command" $
        let initialState = Lib2.State [Lib2.Route "route1" [] []]
            command = Lib2.RouteGet "route1"
        in Lib2.stateTransition initialState command @?= Right (["Route {routeId = \"route1\", stops = [], nestedRoutes = []}"], initialState),

    testCase "RouteAddRoute command" $
        let initialState = Lib2.State [Lib2.Route "parent" [] []]
            parentRoute = Lib2.Route "parent" [] []
            childRoute = Lib2.Route "child" [] []
            command = Lib2.RouteAddRoute parentRoute childRoute
            expectedState = Lib2.State [Lib2.Route "parent" [] [childRoute]]
        in Lib2.stateTransition initialState command @?= Right (["Added route to parent route"], expectedState),

    testCase "RouteRemove command" $
        let initialState = Lib2.State [Lib2.Route "route1" [] []]
            command = Lib2.RouteRemove "route1"
            expectedState = Lib2.State []
        in Lib2.stateTransition initialState command @?= Right (["Removed route"], expectedState),

    testCase "StopCreate command" $
        let initialState = Lib2.State []
            command = Lib2.StopCreate "stop1"
        in Lib2.stateTransition initialState command @?= Right (["Created stop"], initialState),

    testCase "StopDelete command" $
        let initialState = Lib2.State [Lib2.Route "route1" [Lib2.Stop "stop1"] []]
            command = Lib2.StopDelete "stop1"
            expectedState = Lib2.State [Lib2.Route "route1" [] []]
        in Lib2.stateTransition initialState command @?= Right (["Deleted stop"], expectedState)
    
    ]