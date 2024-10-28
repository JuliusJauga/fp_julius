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
import System.IO (withFile, IOMode(..), hPutStrLn, Handle)

-- Helper function to print the state
printState :: Handle -> State -> IO ()
printState handle (State routeTreeLists' routes' stops'') = do
    hPutStrLn handle "Route Tree Lists:"
    mapM_ (printRouteTreeList handle 0) routeTreeLists'
    hPutStrLn handle "Routes:"
    mapM_ (printRoute handle 0) routes'
    hPutStrLn handle "Stops:"
    mapM_ (printStop handle) stops''
    hPutStrLn handle ""

printRouteTreeList :: Handle -> Int -> (Name, [RouteTree]) -> IO ()
printRouteTreeList handle indentLevel (name, trees) = do
    hPutStrLn handle $ replicate indentLevel ' ' ++ "List Name: " ++ show name
    mapM_ (printRouteTree handle (indentLevel + 2)) trees

printRouteTree :: Handle -> Int -> RouteTree -> IO ()
printRouteTree handle indentLevel EmptyTree = hPutStrLn handle $ replicate indentLevel ' ' ++ "Empty Tree"
printRouteTree handle indentLevel (Node (NodeRoute routeId stops'') children) = do
    hPutStrLn handle $ replicate indentLevel ' ' ++ "Route ID: " ++ show routeId
    hPutStrLn handle $ replicate indentLevel ' ' ++ "Stops: " ++ show stops''
    hPutStrLn handle $ replicate indentLevel ' ' ++ "Children:"
    mapM_ (printRouteTree handle (indentLevel + 2)) children

printRoute :: Handle -> Int -> Route -> IO ()
printRoute handle indentLevel (Route routeId stops'' nestedRoutes) = do
    hPutStrLn handle $ replicate indentLevel ' ' ++ "Route ID: " ++ show routeId
    hPutStrLn handle $ replicate indentLevel ' ' ++ "Stops: " ++ show stops''
    hPutStrLn handle $ replicate indentLevel ' ' ++ "Nested Routes:"
    mapM_ (printRoute handle (indentLevel + 2)) nestedRoutes

printStop :: Handle -> Stop -> IO ()
printStop handle (Stop stopId) = hPutStrLn handle $ "  Stop ID: " ++ show stopId

main :: IO ()
main = withFile "lab2_example.txt" WriteMode $ \handle -> do
    -- Initial state
    let state1 = initialState
    hPutStrLn handle "Initial State:"
    printState handle state1
    -- Expected output:
    -- Route Tree Lists:
    -- Routes:
    -- Stops:

    -- Create a list
    let state2 = stateTransition state1 (ListCreate (StringName "List1"))
    hPutStrLn handle "After ListCreate 'List1':"
    printState handle state2
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    -- Stops:

    -- Create routes
    let state3 = stateTransition state2 (RouteCreate (StringName "Route1"))
    let state4 = stateTransition state3 (RouteCreate (StringName "Route2"))
    let state5 = stateTransition state4 (RouteCreate (StringName "Route3"))
    hPutStrLn handle "After RouteCreate 'Route1', 'Route2', 'Route3':"
    printState handle state5
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
    hPutStrLn handle "After RouteAddStop 'Route1' 'Stop1', 'Route2' 'Stop2', 'Route3' 'Stop3':"
    printState handle state8
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = StringName "Stop1"}]
    --   Nested Routes:
    --   Route ID: Route2
    --   Stops: [Stop {stopId' = StringName "Stop2"}]
    --   Nested Routes:
    --   Route ID: Route3
    --   Stops: [Stop {stopId' = StringName "Stop3"}]
    --   Nested Routes:
    -- Stops:

    -- Add nested routes
    let state9 = stateTransition state8 (RouteAddRoute (Route (StringName "Route1") [] []) (Route (StringName "Route2") [] []))
    let state10 = stateTransition state9 (RouteAddRoute (Route (StringName "Route2") [] []) (Route (StringName "Route3") [] []))
    hPutStrLn handle "After RouteAddRoute 'Route1' -> 'Route2', 'Route2' -> 'Route3':"
    printState handle state10
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = StringName "Stop1"}]
    --   Nested Routes:
    --     Route ID: Route2
    --     Stops: [Stop {stopId' = StringName "Stop2"}]
    --     Nested Routes:
    --       Route ID: Route3
    --       Stops: [Stop {stopId' = StringName "Stop3"}]
    --       Nested Routes:
    -- Stops:

    -- Add the nested route to the list
    let state11 = stateTransition state10 (ListAdd (StringName "List1") (Route (StringName "Route1") [] [Route (StringName "Route2") [] [Route (StringName "Route3") [] []]]))
    hPutStrLn handle "After ListAdd 'List1' with nested routes:"
    printState handle state11
    -- Expected output:
    -- Route Tree Lists:
    --   List Name: List1
    --     Route ID: Route1
    --     Stops: [Stop {stopId' = StringName "Stop1"}]
    --     Children:
    --       Route ID: Route2
    --       Stops: [Stop {stopId' = StringName "Stop2"}]
    --       Children:
    --         Route ID: Route3
    --         Stops: [Stop {stopId' = StringName "Stop3"}]
    --         Children:
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = StringName "Stop1"}]
    --   Nested Routes:
    --     Route ID: Route2
    --     Stops: [Stop {stopId' = StringName "Stop2"}]
    --     Nested Routes:
    --       Route ID: Route3
    --       Stops: [Stop {stopId' = StringName "Stop3"}]
    --       Nested Routes:
    -- Stops:

    -- Remove the list
    let state12 = stateTransition state11 (ListRemove (StringName "List1"))
    hPutStrLn handle "After ListRemove 'List1':"
    printState handle state12
    -- Expected output:
    -- Route Tree Lists:
    -- Routes:
    --   Route ID: Route1
    --   Stops: [Stop {stopId' = StringName "Stop1"}]
    --   Nested Routes:
    --     Route ID: Route2
    --     Stops: [Stop {stopId' = StringName "Stop2"}]
    --     Nested Routes:
    --       Route ID: Route3
    --       Stops: [Stop {stopId' = StringName "Stop3"}]
    --       Nested Routes:
    -- Stops: