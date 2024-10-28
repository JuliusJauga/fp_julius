module Main where

import Lib2 (
    Stop(..),
    Route(..),
    Query(..),
    State(..),
    RouteTree(..),
    NodeRoute(..),
    Name(..),
    stateTransition,
    initialState
    )

-- Helper function to print the state
printState :: State -> IO ()
printState (State routeTreeLists' routes' stops'') = do
    putStrLn "Route Tree Lists:"
    mapM_ (printRouteTreeList 0) routeTreeLists'
    putStrLn "Routes:"
    mapM_ (printRoute 0) routes'
    putStrLn "Stops:"
    mapM_ printStop stops''
    putStrLn ""

printRouteTreeList :: Int -> (Name, [RouteTree]) -> IO ()
printRouteTreeList indentLevel (name, trees) = do
    putStrLn $ replicate indentLevel ' ' ++ "List Name: " ++ show name
    mapM_ (printRouteTree (indentLevel + 2)) trees

printRouteTree :: Int -> RouteTree -> IO ()
printRouteTree indentLevel EmptyTree = putStrLn $ replicate indentLevel ' ' ++ "Empty Tree"
printRouteTree indentLevel (Node (NodeRoute routeId stops'') children) = do
    putStrLn $ replicate indentLevel ' ' ++ "Route ID: " ++ show routeId
    putStrLn $ replicate indentLevel ' ' ++ "Stops: " ++ show stops''
    putStrLn $ replicate indentLevel ' ' ++ "Children:"
    mapM_ (printRouteTree (indentLevel + 2)) children

printRoute :: Int -> Route -> IO ()
printRoute indentLevel (Route routeId stops'' nestedRoutes) = do
    putStrLn $ replicate indentLevel ' ' ++ "Route ID: " ++ show routeId
    putStrLn $ replicate indentLevel ' ' ++ "Stops: " ++ show stops''
    putStrLn $ replicate indentLevel ' ' ++ "Nested Routes:"
    mapM_ (printRoute (indentLevel + 2)) nestedRoutes

printStop :: Stop -> IO ()
printStop (Stop stopId) = putStrLn $ "  Stop ID: " ++ show stopId


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
    let state2 = stateTransition state1 (ListCreate (StringName "List1"))
    putStrLn "After ListCreate 'List1':"
    printState state2
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    -- Stops:

    -- Create routes
    let state3 = stateTransition state2 (RouteCreate (StringName "Route1"))
    let state4 = stateTransition state3 (RouteCreate (StringName "Route2"))
    let state5 = stateTransition state4 (RouteCreate (StringName "Route3"))
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
    let state6 = stateTransition state5 (RouteAddStop (StringName "Route1") (Stop (StringName "Stop1")))
    let state7 = stateTransition state6 (RouteAddStop (StringName "Route2") (Stop (StringName "Stop2")))
    let state8 = stateTransition state7 (RouteAddStop (StringName "Route3") (Stop (StringName "Stop3")))
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
    let state9 = stateTransition state8 (RouteAddRoute (Route (StringName "Route1") [] []) (Route (StringName "Route2") [] []))
    let state10 = stateTransition state9 (RouteAddRoute (Route (StringName "Route2") [] []) (Route (StringName "Route3") [] []))
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
    let state11 = stateTransition state10 (ListAdd (StringName "List1") (Route (StringName "Route1") [] [Route (StringName "Route2") [] [Route (StringName "Route3") [] []]]))
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
    let state12 = stateTransition state11 (ListRemove (StringName "List1"))
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