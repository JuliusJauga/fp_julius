{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified (
        Stop(..),
        Route(..),
        Query(..),
        State(..),
        RouteTree(..),
        NodeRoute(..),
        parseQuery,
        parseRouteSystem,
        stateTransition,
        initialState,
        singletonRoute,
        insertRoute,
        insertNestedRoutes,
        rebuildRoute,
        nodeStopsFromTree,
        parseListAdd,
        parseListCreate,
        parseListGet,
        parseListRemove,
        parseRouteCreate,
        parseRouteGet,
        parseRouteAddRoute,
        parseRouteRemove,
        parseStopCreate,
        parseStopDelete,
        parseStop,
        parseRouteList,
        parseStopList,
        parseRoute
        )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests" [
    -- Parsing Tests
    testCase "parseStop parses a valid stop" $
        Lib2.parseStop "(Stop1)" @?= Right (Lib2.Stop "Stop1", ""),

    testCase "parseStop fails on invalid stop" $
        Lib2.parseStop "Stop1)" @?= Left "Expected '(' at the start of stop.",

    testCase "parseStopList parses a valid stop list" $
        Lib2.parseStopList "(Stop1)(Stop2)" @?= Right ([Lib2.Stop "Stop1", Lib2.Stop "Stop2"], ""),

    testCase "parseStopList fails on invalid stop list" $
        Lib2.parseStopList "(Stop1)Stop2)" @?= Right ([Lib2.Stop "Stop1"], "Stop2)"),

    testCase "parseRoute parses a valid route" $
        Lib2.parseRoute "<Route1{(Stop1)(Stop2)}>" @?= Right (Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] [], ""),

    testCase "parseRoute fails on invalid route" $
        Lib2.parseRoute "Route1{(Stop1)(Stop2)}>" @?= Left "Expected '<' at the start of route.",

    testCase "parseRouteList parses a valid route list" $
        Lib2.parseRouteList "<Route1{(Stop1)(Stop2)}><Route2{(Stop3)}><Route3{(Stop4)(Stop5)}>" @?= Right ([Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] [], Lib2.Route "Route2" [Lib2.Stop "Stop3"] [], Lib2.Route "Route3" [Lib2.Stop "Stop4", Lib2.Stop "Stop5"] []], ""),

    testCase "parseRouteList fails on invalid route list" $
        Lib2.parseRouteList "<Route1{(Stop1)(Stop2)}><Route2{(Stop3)}Route3{(Stop4)(Stop5)}>" @?= Right ([Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] [], Lib2.Route "Route2" [Lib2.Stop "Stop3"] []], "Route3{(Stop4)(Stop5)}>"),

    testCase "parseRouteSystem parses a valid route system" $
        Lib2.parseRouteSystem "[<Route1{(Stop1)(Stop2)}><Route2{(Stop3)}><Route3{(Stop4)(Stop5)}>]" @?= Right ([Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] [], Lib2.Route "Route2" [Lib2.Stop "Stop3"] [], Lib2.Route "Route3" [Lib2.Stop "Stop4", Lib2.Stop "Stop5"] []], ""),

    testCase "parseRouteSystem fails on invalid route system" $
        Lib2.parseRouteSystem "[<Route1{(Stop1)(Stop2)}><Route2{(Stop3)}Route3{(Stop4)(Stop5)}>]" @?= Left "Failed to parse route system.",

    testCase "parseListCreate parses a valid list-create query" $
        Lib2.parseListCreate "list-create List1" @?= Right (Lib2.ListCreate "List1", ""),

    testCase "parseListCreate fails on invalid list-create query" $
        Lib2.parseListCreate "list-createList1" @?= Left "Expected 'list-create'.",

    testCase "parseListAdd parses a valid list-add query" $
        Lib2.parseListAdd "list-add List1 <Route1{(Stop1)(Stop2)}>" @?= Right (Lib2.ListAdd "List1" (Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] []), ""),

    testCase "parseListAdd fails on invalid list-add query" $
        Lib2.parseListAdd "list-add List1 Route1{(Stop1)(Stop2)}>" @?= Left "Expected a valid route.",

    testCase "parseListGet parses a valid list-get query" $
        Lib2.parseListGet "list-get List1" @?= Right (Lib2.ListGet "List1", ""),

    testCase "parseListGet fails on invalid list-get query" $
        Lib2.parseListGet "list-getList1" @?= Left "Expected 'list-get'.",

    testCase "parseListRemove parses a valid list-remove query" $
        Lib2.parseListRemove "list-remove List1" @?= Right (Lib2.ListRemove "List1", ""),

    testCase "parseListRemove fails on invalid list-remove query" $
        Lib2.parseListRemove "list-removeList1" @?= Left "Expected 'list-remove'.",

    testCase "parseRouteCreate parses a valid route-create query" $
        Lib2.parseRouteCreate "route-create Route1" @?= Right (Lib2.RouteCreate "Route1", ""),

    testCase "parseRouteCreate fails on invalid route-create query" $
        Lib2.parseRouteCreate "route-createRoute1" @?= Left "Expected 'route-create'.",

    testCase "parseRouteGet parses a valid route-get query" $
        Lib2.parseRouteGet "route-get Route1" @?= Right (Lib2.RouteGet "Route1", ""),

    testCase "parseRouteGet fails on invalid route-get query" $
        Lib2.parseRouteGet "route-getRoute1" @?= Left "Expected 'route-get'.",

    testCase "parseRouteAddRoute parses a valid route-add-route query" $
        Lib2.parseRouteAddRoute "route-add-route <Route1{(Stop1)(Stop2)}> <Route2{(Stop3)}>" @?= Right (Lib2.RouteAddRoute (Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] []) (Lib2.Route "Route2" [Lib2.Stop "Stop3"] []), ""),

    testCase "parseRouteAddRoute fails on invalid route-add-route query" $
        Lib2.parseRouteAddRoute "route-add-route Route1{(Stop1)(Stop2)}> <Route2{(Stop3)}>" @?= Left "Expected a valid parent route.",

    testCase "parseRouteRemove parses a valid route-remove query" $
        Lib2.parseRouteRemove "route-remove Route1" @?= Right (Lib2.RouteRemove "Route1", ""),

    testCase "parseRouteRemove fails on invalid route-remove query" $
        Lib2.parseRouteRemove "route-removeRoute1" @?= Left "Expected 'route-remove'.",

    testCase "parseStopCreate parses a valid stop-create query" $
        Lib2.parseStopCreate "stop-create Stop1" @?= Right (Lib2.StopCreate "Stop1", ""),

    testCase "parseStopCreate fails on invalid stop-create query" $
        Lib2.parseStopCreate "stop-createStop1" @?= Left "Expected 'stop-create'.",

    testCase "parseStopDelete parses a valid stop-delete query" $
        Lib2.parseStopDelete "stop-delete Stop1" @?= Right (Lib2.StopDelete "Stop1", ""),

    testCase "parseStopDelete fails on invalid stop-delete query" $
        Lib2.parseStopDelete "stop-deleteStop1" @?= Left "Expected 'stop-delete'.",
    
    -- State Transition Tests
    testCase "stateTransition handles ListCreate" $
        Lib2.stateTransition Lib2.initialState (Lib2.ListCreate "List1") @?= Lib2.State [("List1", [])] [] [],

    testCase "stateTransition handles ListAdd" $
        Lib2.stateTransition (Lib2.State [("List1", [])] [Lib2.Route "Route1" [] []] []) (Lib2.ListAdd "List1" (Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] [])) @?= Lib2.State [("List1", [Lib2.Node (Lib2.NodeRoute "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"]) []])] [] [],

    testCase "stateTransition handles ListRemove" $
        Lib2.stateTransition (Lib2.State [("List1", [Lib2.Node (Lib2.NodeRoute "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"]) []])] [Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] []] []) (Lib2.ListRemove "List1") @?= Lib2.State [] [Lib2.Route "Route1" [Lib2.Stop "Stop1", Lib2.Stop "Stop2"] []] [],

    testCase "stateTransition handles RouteCreate" $
        Lib2.stateTransition Lib2.initialState (Lib2.RouteCreate "Route1") @?= Lib2.State [] [Lib2.Route "Route1" [] []] [],

    testCase "stateTransition handles RouteAddRoute" $
        Lib2.stateTransition (Lib2.State [] [Lib2.Route "Route1" [] [], Lib2.Route "Route2" [] []] []) (Lib2.RouteAddRoute (Lib2.Route "Route1" [] []) (Lib2.Route "Route2" [] [])) @?= Lib2.State [] [Lib2.Route "Route1" [] [Lib2.Route "Route2" [] []]] [],

    testCase "stateTransition handles RouteRemove" $
        Lib2.stateTransition (Lib2.State [] [Lib2.Route "Route1" [] [], Lib2.Route "Route2" [] []] []) (Lib2.RouteRemove "Route1") @?= Lib2.State [] [Lib2.Route "Route2" [] []] [],

    testCase "stateTransition handles RouteAddStop" $
        Lib2.stateTransition (Lib2.State [] [Lib2.Route "Route1" [] []] []) (Lib2.RouteAddStop "Route1" (Lib2.Stop "Stop1")) @?= Lib2.State [] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] []] [],

    testCase "stateTransition handles RouteRemoveStop" $
        Lib2.stateTransition (Lib2.State [] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] []] []) (Lib2.RouteRemoveStop "Route1" (Lib2.Stop "Stop1")) @?= Lib2.State [] [Lib2.Route "Route1" [] []] [Lib2.Stop "Stop1"],

    testCase "stateTransition handles StopCreate" $
        Lib2.stateTransition Lib2.initialState (Lib2.StopCreate "Stop1") @?= Lib2.State [] [] [Lib2.Stop "Stop1"],

    testCase "stateTransition handles StopDelete" $
        Lib2.stateTransition (Lib2.State [] [] [Lib2.Stop "Stop1"]) (Lib2.StopDelete "Stop1") @?= Lib2.State [] [] [],

    -- Additional State Transition Tests
    testCase "stateTransition handles ListAdd with non-existing list" $
        Lib2.stateTransition Lib2.initialState (Lib2.ListAdd "NonExistingList" (Lib2.Route "Route1" [Lib2.Stop "Stop1"] [])) @?= Lib2.initialState,

    testCase "stateTransition handles RouteAddRoute with non-existing parent route" $
        Lib2.stateTransition Lib2.initialState (Lib2.RouteAddRoute (Lib2.Route "NonExistingRoute" [] []) (Lib2.Route "Route1" [Lib2.Stop "Stop1"] [])) @?= Lib2.initialState,

    testCase "stateTransition handles RouteAddStop with non-existing route" $
        Lib2.stateTransition Lib2.initialState (Lib2.RouteAddStop "NonExistingRoute" (Lib2.Stop "Stop1")) @?= Lib2.initialState,

    testCase "stateTransition handles RouteRemoveStop with non-existing route" $
        Lib2.stateTransition Lib2.initialState (Lib2.RouteRemoveStop "NonExistingRoute" (Lib2.Stop "Stop1")) @?= Lib2.initialState,

    testCase "stateTransition handles StopDelete with non-existing stop" $
        Lib2.stateTransition Lib2.initialState (Lib2.StopDelete "NonExistingStop") @?= Lib2.initialState,
    
    -- Higher Recursion Tests
    testCase "stateTransition handles nested RouteAddRoute" $
        let initialState = Lib2.State [] [Lib2.Route "Route1" [] [], Lib2.Route "Route2" [] [], Lib2.Route "Route3" [] []] []
            expectedState = Lib2.State [] [Lib2.Route "Route1" [] [Lib2.Route "Route2" [] [Lib2.Route "Route3" [] []]]] []
        in Lib2.stateTransition (Lib2.stateTransition initialState (Lib2.RouteAddRoute (Lib2.Route "Route1" [] []) (Lib2.Route "Route2" [] []))) (Lib2.RouteAddRoute (Lib2.Route "Route2" [] []) (Lib2.Route "Route3" [] [])) @?= expectedState,

    testCase "stateTransition handles nested ListAdd" $
        let initialState = Lib2.State [("List1", [])] [Lib2.Route "Route1" [] [Lib2.Route "Route2" [] [Lib2.Route "Route3" [] []]]] []
            expectedState = Lib2.State [("List1", [Lib2.Node (Lib2.NodeRoute "Route1" []) [Lib2.Node (Lib2.NodeRoute "Route2" []) [Lib2.Node (Lib2.NodeRoute "Route3" []) []]]])] [] []
        in Lib2.stateTransition initialState (Lib2.ListAdd "List1" (Lib2.Route "Route1" [] [Lib2.Route "Route2" [] [Lib2.Route "Route3" [] []]])) @?= expectedState,

    testCase "stateTransition handles ListRemove with nested routes" $
        let initialState = Lib2.State [("List1", [Lib2.Node (Lib2.NodeRoute "Route1" []) [Lib2.Node (Lib2.NodeRoute "Route2" []) [Lib2.Node (Lib2.NodeRoute "Route3" []) []]]])] [Lib2.Route "Route1" [] [Lib2.Route "Route2" [] [Lib2.Route "Route3" [] []]]] []
            expectedState = Lib2.State [] [Lib2.Route "Route1" [] [Lib2.Route "Route2" [] [Lib2.Route "Route3" [] []]]] []
        in Lib2.stateTransition initialState (Lib2.ListRemove "List1") @?= expectedState,
    
    testCase "stateTransition handles nested RouteAddRoute with stops" $
        let initialState = Lib2.State [] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] [], Lib2.Route "Route2" [Lib2.Stop "Stop2"] [], Lib2.Route "Route3" [Lib2.Stop "Stop3"] []] []
            expectedState = Lib2.State [] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] [Lib2.Route "Route2" [Lib2.Stop "Stop2"] [Lib2.Route "Route3" [Lib2.Stop "Stop3"] []]]] []
        in Lib2.stateTransition (Lib2.stateTransition initialState (Lib2.RouteAddRoute (Lib2.Route "Route1" [Lib2.Stop "Stop1"] []) (Lib2.Route "Route2" [Lib2.Stop "Stop2"] []))) (Lib2.RouteAddRoute (Lib2.Route "Route2" [Lib2.Stop "Stop2"] []) (Lib2.Route "Route3" [Lib2.Stop "Stop3"] [])) @?= expectedState,

    testCase "stateTransition handles nested ListAdd with stops" $
        let initialState = Lib2.State [("List1", [])] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] [Lib2.Route "Route2" [Lib2.Stop "Stop2"] [Lib2.Route "Route3" [Lib2.Stop "Stop3"] []]]] []
            expectedState = Lib2.State [("List1", [Lib2.Node (Lib2.NodeRoute "Route1" [Lib2.Stop "Stop1"]) [Lib2.Node (Lib2.NodeRoute "Route2" [Lib2.Stop "Stop2"]) [Lib2.Node (Lib2.NodeRoute "Route3" [Lib2.Stop "Stop3"]) []]]])] [] []
        in Lib2.stateTransition initialState (Lib2.ListAdd "List1" (Lib2.Route "Route1" [Lib2.Stop "Stop1"] [Lib2.Route "Route2" [Lib2.Stop "Stop2"] [Lib2.Route "Route3" [Lib2.Stop "Stop3"] []]])) @?= expectedState,

    testCase "stateTransition handles ListRemove with nested routes and stops" $
        let initialState = Lib2.State [("List1", [Lib2.Node (Lib2.NodeRoute "Route1" [Lib2.Stop "Stop1"]) [Lib2.Node (Lib2.NodeRoute "Route2" [Lib2.Stop "Stop2"]) [Lib2.Node (Lib2.NodeRoute "Route3" [Lib2.Stop "Stop3"]) []]]])] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] [Lib2.Route "Route2" [Lib2.Stop "Stop2"] [Lib2.Route "Route3" [Lib2.Stop "Stop3"] []]]] []
            expectedState = Lib2.State [] [Lib2.Route "Route1" [Lib2.Stop "Stop1"] [Lib2.Route "Route2" [Lib2.Stop "Stop2"] [Lib2.Route "Route3" [Lib2.Stop "Stop3"] []]]] []
        in Lib2.stateTransition initialState (Lib2.ListRemove "List1") @?= expectedState
    
    ]