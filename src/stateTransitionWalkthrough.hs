module Main where

import Lib2 (
    Stop(..),
    Route(..),
    Query(..),
    State(..),
    RouteTree(..),
    NodeRoute(..),
    stateTransition,
    initialState
    )

-- Helper function to print the state
printState :: State -> IO ()
printState (State routeTreeLists' routes' stops'') = do
    putStrLn "Route Tree Lists:"
    mapM_ printRouteTreeList routeTreeLists'
    putStrLn "Routes:"
    mapM_ printRoute routes'
    putStrLn "Stops:"
    mapM_ printStop stops''
    putStrLn ""

printRouteTreeList :: (String, [RouteTree]) -> IO ()
printRouteTreeList (name, trees) = do
    putStrLn $ "  List Name: " ++ name
    mapM_ printRouteTree trees

printRouteTree :: RouteTree -> IO ()
printRouteTree EmptyTree = putStrLn "    Empty Tree"
printRouteTree (Node (NodeRoute routeId stops'') children) = do
    putStrLn $ "    Route ID: " ++ routeId
    putStrLn $ "    Stops: " ++ show stops''
    putStrLn "    Children:"
    mapM_ printRouteTree children

printRoute :: Route -> IO ()
printRoute (Route routeId stops'' nestedRoutes) = do
    putStrLn $ "  Route ID: " ++ routeId
    putStrLn $ "  Stops: " ++ show stops''
    putStrLn "  Nested Routes:"
    mapM_ printRoute nestedRoutes

printStop :: Stop -> IO ()
printStop (Stop stopId) = putStrLn $ "  Stop ID: " ++ stopId

main :: IO ()
main = do
    -- Initial state
    let state1 = initialState
    putStrLn "Initial State:"
    printState state1
    -- Expected output:
    -- Route Tree Lists:
    -- Routes:
    -- Stops:

    -- Create a list
    let state2 = stateTransition state1 (ListCreate "List1")
    putStrLn "After ListCreate 'List1':"
    printState state2
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    -- Stops:

    -- Create routes
    let state3 = stateTransition state2 (RouteCreate "Route1")
    let state4 = stateTransition state3 (RouteCreate "Route2")
    let state5 = stateTransition state4 (RouteCreate "Route3")
    putStrLn "After RouteCreate 'Route1', 'Route2', 'Route3':"
    printState state5
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    --   Route ID: Route1
    --   Stops: []
    --   Nested Routes:
    --   Route ID: Route2
    --   Stops: []
    --   Nested Routes:
    --   Route ID: Route3
    --   Stops: []
    --   Nested Routes:
    -- Stops:

    -- Add stops to routes
    let state6 = stateTransition state5 (RouteAddStop "Route1" (Stop "Stop1"))
    let state7 = stateTransition state6 (RouteAddStop "Route2" (Stop "Stop2"))
    let state8 = stateTransition state7 (RouteAddStop "Route3" (Stop "Stop3"))
    putStrLn "After RouteAddStop 'Route1' 'Stop1', 'Route2' 'Stop2', 'Route3' 'Stop3':"
    printState state8
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = "Stop1"}]
    --   Nested Routes:
    --   Route ID: Route2
    --   Stops: [Stop {stopId' = "Stop2"}]
    --   Nested Routes:
    --   Route ID: Route3
    --   Stops: [Stop {stopId' = "Stop3"}]
    --   Nested Routes:
    -- Stops:

    -- Add nested routes
    let state9 = stateTransition state8 (RouteAddRoute (Route "Route1" [] []) (Route "Route2" [] []))
    let state10 = stateTransition state9 (RouteAddRoute (Route "Route2" [] []) (Route "Route3" [] []))
    putStrLn "After RouteAddRoute 'Route1' -> 'Route2', 'Route2' -> 'Route3':"
    printState state10
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = "Stop1"}]
    --   Nested Routes:
    --     Route ID: Route2
    --     Stops: [Stop {stopId' = "Stop2"}]
    --     Nested Routes:
    --       Route ID: Route3
    --       Stops: [Stop {stopId' = "Stop3"}]
    --       Nested Routes:
    -- Stops:

    -- Add the nested route to the list
    let state11 = stateTransition state10 (ListAdd "List1" (Route "Route1" [] [Route "Route2" [] [Route "Route3" [] []]]))
    putStrLn "After ListAdd 'List1' with nested routes:"
    printState state11
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    --     Route ID: Route1
    --     Stops: [Stop {stopId' = "Stop1"}]
    --     Children:
    --       Route ID: Route2
    --       Stops: [Stop {stopId' = "Stop2"}]
    --       Children:
    --         Route ID: Route3
    --         Stops: [Stop {stopId' = "Stop3"}]
    --         Children:
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = "Stop1"}]
    --   Nested Routes:
    --     Route ID: Route2
    --     Stops: [Stop {stopId' = "Stop2"}]
    --     Nested Routes:
    --       Route ID: Route3
    --       Stops: [Stop {stopId' = "Stop3"}]
    --       Nested Routes:
    -- Stops:

    -- Remove the list
    let state12 = stateTransition state11 (ListRemove "List1")
    putStrLn "After ListRemove 'List1':"
    printState state12
    -- Expected output:
    -- Route Tree Lists:
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = "Stop1"}]
    --   Nested Routes:
    --     Route ID: Route2
    --     Stops: [Stop {stopId' = "Stop2"}]
    --     Nested Routes:
    --       Route ID: Route3
    --       Stops: [Stop {stopId' = "Stop3"}]
    --       Nested Routes:
    -- Stops: