module Lib2 (
        Stop(..),
        Route(..),
        Query(..),
        State(..),
        RouteTree(..),
        NodeRoute(..),
        parseQuery,
        parseRouteSystem,
        stateTransition,
        initialState
    ) where

    import Data.Char (isSpace, isAlphaNum)
    import Data.List (isPrefixOf, partition)

    -- Parsing components

    -- Helper function to parse many elements using another parser
    parseMany :: (String -> Either String (a, String)) -> String -> Either String ([a], String)
    parseMany parser input = case parser input of
        Right (x, rest) ->
            case parseMany parser rest of
                Right (xs, rest') -> Right (x:xs, rest')
                Left _ -> Right ([x], rest)
        Left _ -> Right ([], input)

    -- Parse a single character
    -- <char> ::= "a" | "b" | "c" ...
    char :: Char -> String -> Either String (Char, String)
    char c (x:xs)
        | c == x = Right (c, xs)
        | otherwise = Left $ "Expected '" ++ [c] ++ "'"
    char _ [] = Left "Unexpected end of input"

    -- Parse a name
    -- <name> ::= <char>+ | <name> " " <name>
    name :: String -> Either String (String, String)
    name input =
        let (n, rest) = span (\c -> isAlphaNum c || c == ' ') input
            trimmedName = reverse (dropWhile isSpace (reverse (dropWhile isSpace n)))
        in if null trimmedName then Left "Expected a name, but found none"
        else Right (trimmedName, rest)

    -- Parse a stop
    -- <stop> ::= "(" <stop_id> ")"
    parseStop :: String -> Either String (Stop, String)
    parseStop input =
        case char '(' input of
            Right (_, rest1) ->
                case name rest1 of
                    Right (stopId'', rest2) ->
                        case char ')' rest2 of
                            Right (_, rest3) -> Right (Stop stopId'', rest3)
                            Left _ -> Left "Expected ')' at the end of stop."
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected '(' at the start of stop."

    -- Parse a list of stops
    -- <stop_list> ::= <stop>*
    parseStopList :: String -> Either String ([Stop], String)
    parseStopList = parseMany parseStop

    -- Parse a route
    -- <route> ::= "<" <route_id> "{" <stop_list> <nested_route_list> "}" ">"
    parseRoute :: String -> Either String (Route, String)
    parseRoute input =
        case char '<' input of
            Right (_, rest1) ->
                case name rest1 of
                    Right (routeId'', rest2) ->
                        case char '{' rest2 of
                            Right (_, rest3) ->
                                case parseStopList rest3 of
                                    Right (stops'', rest4) ->
                                        case parseRouteList rest4 of
                                            Right (nestedRoutes, rest5) ->
                                                case char '}' rest5 of
                                                    Right (_, rest6) ->
                                                        case char '>' rest6 of
                                                            Right (_, rest7) -> Right (Route routeId'' stops'' nestedRoutes, rest7)
                                                            Left _ -> Right (Route routeId'' stops'' nestedRoutes, rest6)
                                                    Left _ -> Left "Expected '}' at the end of stops and nested routes."
                                            Left _ -> Left "Failed to parse nested routes."
                                    Left _ -> Left "Failed to parse stops."
                            Left _ -> Left "Expected '{' at the start of stops and nested routes."
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected '<' at the start of route."

    -- Parse a list of routes
    -- <route_list_internal> ::= <route>*
    parseRouteList :: String -> Either String ([Route], String)
    parseRouteList input =
        case char '[' input of
            Right (_, rest1) ->
                case parseMany parseRoute rest1 of
                    Right (routes'', rest2) ->
                        case char ']' rest2 of
                            Right (_, rest3) -> Right (routes'', rest3)
                            Left _ -> Left "Expected ']' at the end of route list."
                    Left _ -> Left "Failed to parse route list."
            Left _ -> Left "Expected '[' at the start of route list."

    -- Parse a route system
    -- <route_list_internal> ::= <route>*
    parseRouteSystem :: String -> Either String ([Route], String)
    parseRouteSystem input =
        case parseRouteList input of
            Right (routes'', rest) -> Right (routes'', rest)
            Left _ -> Left "Failed to parse route system."

    -- Helper function to parse a string. Just to parse querys
    string :: String -> String -> Either String (String, String)
    string str input
        | str `isPrefixOf` input = Right (str, drop (length str) input)
        | otherwise = Left $ "Expected '" ++ str ++ "'"



    -- Parsing functions

    -- Parse a list-create query
    -- <list_create> ::= "list-create " <name>
    parseListCreate :: String -> Either String (Query, String)
    parseListCreate input =
        case string "list-create " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest') -> Right (ListCreate listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-create'."

    -- Parse a list-add query
    -- <list_add> ::= "list-add " <name> <route>
    parseListAdd :: String -> Either String (Query, String)
    parseListAdd input =
        case string "list-add " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest1) ->
                        case parseRoute rest1 of
                            Right (route, rest2) -> Right (ListAdd listName route, rest2)
                            Left _ -> Left "Expected a valid route."
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-add'."

    -- Parse a list-get query
    -- <list_get> ::= "list-get " <name>
    parseListGet :: String -> Either String (Query, String)
    parseListGet input =
        case string "list-get " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest') -> Right (ListGet listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-get'."

    -- Parse a list-remove query
    -- <list_remove> ::= "list-remove " <name>
    parseRouteCreate :: String -> Either String (Query, String)
    parseRouteCreate input =
        case string "route-create " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest') -> Right (RouteCreate routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-create'."

    -- Parse a route-get query
    -- <route_get> ::= "route-get " <name>
    parseRouteGet :: String -> Either String (Query, String)
    parseRouteGet input =
        case string "route-get " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest') -> Right (RouteGet routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-get'."

    -- Parse a route-add-route query
    -- <route_add_route> ::= "route-add-route " <route> <route>
    parseRouteAddRoute :: String -> Either String (Query, String)
    parseRouteAddRoute input =
        case string "route-add-route " input of
            Right (_, rest) ->
                case parseRoute rest of
                    Right (parentRoute, rest1) ->
                        case string " " rest1 of
                            Right (_, rest2) ->
                                case parseRoute rest2 of
                                    Right (childRoute, rest3) -> Right (RouteAddRoute parentRoute childRoute, rest3)
                                    Left _ -> Left "Expected a valid child route."
                            Left _ -> Left "Expected a space between parent and child routes."
                    Left _ -> Left "Expected a valid parent route."
            Left _ -> Left "Expected 'route-add-route'."

    -- Parse a route-remove query
    -- <route_remove> ::= "route-remove " <name>
    parseRouteRemove :: String -> Either String (Query, String)
    parseRouteRemove input =
        case string "route-remove " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest') -> Right (RouteRemove routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-remove'."

    -- Parse a stop-create query
    -- <stop_create> ::= "stop-create " <name>
    parseStopCreate :: String -> Either String (Query, String)
    parseStopCreate input =
        case string "stop-create " input of
            Right (_, rest) ->
                case name rest of
                    Right (stopId, rest') -> Right (StopCreate stopId, rest')
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected 'stop-create'."

    -- Parse a stop-delete query
    -- <stop_delete> ::= "stop-delete " <name>
    parseStopDelete :: String -> Either String (Query, String)
    parseStopDelete input =
        case string "stop-delete " input of
            Right (_, rest) ->
                case name rest of
                    Right (stopId, rest') -> Right (StopDelete stopId, rest')
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected 'stop-delete'."

    -- Main query parser
    parseQuery :: String -> Either String Query
    parseQuery input =
        case parseListCreate input of
            Right (query, _) -> Right query
            Left _ ->
                case parseListAdd input of
                    Right (query, _) -> Right query
                    Left _ ->
                        case parseListGet input of
                            Right (query, _) -> Right query
                            Left _ ->
                                case parseRouteCreate input of
                                    Right (query, _) -> Right query
                                    Left _ ->
                                        case parseRouteGet input of
                                            Right (query, _) -> Right query
                                            Left _ ->
                                                case parseRouteAddRoute input of
                                                    Right (query, _) -> Right query
                                                    Left _ ->
                                                        case parseRouteRemove input of
                                                            Right (query, _) -> Right query
                                                            Left _ ->
                                                                case parseStopCreate input of
                                                                    Right (query, _) -> Right query
                                                                    Left _ ->
                                                                        case parseStopDelete input of
                                                                            Right (query, _) -> Right query
                                                                            Left _ -> Left "Invalid query."


    data Query
        = ListCreate String
        | ListAdd String Route
        | ListGet String
        | ListRemove String
        | RouteCreate String
        | RouteGet String
        | RouteAddRoute Route Route
        | RouteAddStop String Stop
        | RouteRemoveStop String Stop
        | RouteRemove String
        | StopCreate String
        | StopDelete String
        deriving (Show, Eq)

    -- Define the Route data type
    data Route = Route
        { routeId' :: String
        , stops' :: [Stop]
        , nestedRoutes' :: [Route]
        } deriving (Show, Eq)

    -- Define the Stop data type
    newtype Stop = Stop
        { stopId' :: String
        } deriving (Show, Eq)

    -- Define the RouteTree data type
        -- This is a tree structure that represents a route system
        -- Every node in the tree is a Route, and child nodes are nested routes
    
    data RouteTree = EmptyTree | Node NodeRoute [RouteTree]
        deriving (Show, Eq)

    -- Define the NodeRoute data type
    data NodeRoute = NodeRoute
        { nodeRouteId :: String
        , nodeStops :: [Stop]
        } deriving (Show, Eq)

    -- Define the RouteTree functions
    singletonRoute :: Route -> RouteTree
    singletonRoute route = Node (NodeRoute (routeId' route) (stops' route)) []

    -- Define the RouteTree functions
    insertRoute :: Route -> RouteTree -> RouteTree
    insertRoute route EmptyTree =
        let newNode = singletonRoute route
            newNodeWithChildren = insertNestedRoutes newNode (nestedRoutes' route)
        in newNodeWithChildren
    insertRoute route (Node rootRoute childRoutes) =
        let newNode = singletonRoute route
            newNodeWithChildren = insertNestedRoutes newNode (nestedRoutes' route)
        in Node rootRoute (newNodeWithChildren : childRoutes)

    -- Helper function to insert nested routes into a RouteTree 
    insertNestedRoutes :: RouteTree -> [Route] -> RouteTree
    insertNestedRoutes = foldl (flip insertRoute)

    -- Helper function to rebuild a Route from a RouteTree
    rebuildRoute :: RouteTree -> Route
    rebuildRoute EmptyTree = Route "" [] []
    rebuildRoute (Node nodeRoute subTrees) =
        Route (nodeRouteId nodeRoute) (nodeStops nodeRoute) (map rebuildRoute subTrees)


    -- Define the State data type
    data State = State
        { routeTreeLists :: [(String, [RouteTree])],
            routes :: [Route],
            stops :: [Stop]
        } deriving (Show, Eq)


    initialState :: State
    initialState = State [] [] []

    stateTransition :: State -> Query -> State
    stateTransition (State routeLists routes' stops'') (ListCreate listName) =
        State ((listName, []) : routeLists) routes' stops''

    stateTransition (State routeLists routes' stops'') (ListAdd listName route) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId' route) routes'
        in case existingRoutes of
            [] -> State routeLists routes' stops''
            (r:_) ->
                let routeTree = insertRoute r EmptyTree
                    updatedRouteLists = map (\(name', trees) ->
                        if name' == listName
                        then (name', routeTree : trees)
                        else (name', trees)
                        ) routeLists
                in State updatedRouteLists remainingRoutes stops''
    stateTransition (State routeLists routes' stops'') (ListGet _) = State routeLists routes' stops''
    stateTransition (State routeLists routes' stops'') (ListRemove listName) =
        let updatedRouteLists = filter (\(name', _) -> name' /= listName) routeLists
            removedRoutes = concatMap (\(_, trees) -> map rebuildRoute trees) (filter (\(name', _) -> name' == listName) routeLists)
            removedStops = concatMap nodeStopsFromTree (concatMap snd (filter (\(name', _) -> name' == listName) routeLists))
        in State updatedRouteLists (routes' ++ removedRoutes) (stops'' ++ removedStops)
    stateTransition (State routeLists routes' stops'') (RouteCreate routeId) =
        State routeLists (Route routeId [] [] : routes') stops''
    stateTransition (State routeLists routes' stops'') (RouteGet _) = State routeLists routes' stops''
    stateTransition (State routeLists routes' stops'') (RouteAddRoute parentRoute childRoute) =
        let
        remainingRoutes = filter (\r -> routeId' r /= routeId' childRoute) routes'
        updatedRoutes = map (\r ->
                               if routeId' r == routeId' parentRoute
                               then r { nestedRoutes' = nestedRoutes' r ++ [childRoute] }
                               else r
                           ) remainingRoutes
        in State routeLists updatedRoutes stops''
    stateTransition (State routeLists routes' stops'') (RouteRemove routeId) =
        let (removedRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case removedRoutes of
            [] -> State routeLists routes' stops''
            (_:_) -> State routeLists remainingRoutes stops''

    stateTransition (State routeLists routes' stops'') (RouteAddStop routeId stop) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> State routeLists routes' stops''
            (_:_) ->
                let updatedRoutes = map (\r' ->
                        if routeId' r' == routeId
                        then r' { stops' = stops' r' ++ [stop] }
                        else r'
                        ) remainingRoutes
                in State routeLists updatedRoutes stops''
    stateTransition (State routeLists routes' stops'') (RouteRemoveStop routeId stop) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> State routeLists routes' stops''
            (_:_) ->
                let updatedRoutes = map (\r' ->
                        if routeId' r' == routeId
                        then r' { stops' = filter (\s -> stopId' s /= stopId' stop) (stops' r') }
                        else r'
                        ) remainingRoutes
                in State routeLists updatedRoutes stops''
    stateTransition (State routeLists routes' stops'') (StopCreate stopId) = State routeLists routes' (Stop stopId : stops'')
    stateTransition (State routeLists routes' stops'') (StopDelete stopId) =
        let (removedStops, remainingStops) = partition (\s -> stopId' s == stopId) stops''
        in case removedStops of
            [] -> State routeLists routes' stops''
            (_:_) -> State routeLists routes' remainingStops

    -- Helper function to extract all stops from a RouteTree
    nodeStopsFromTree :: RouteTree -> [Stop]
    nodeStopsFromTree EmptyTree = []
    nodeStopsFromTree (Node nodeRoute childRoutes) =
        nodeStops nodeRoute ++ concatMap nodeStopsFromTree childRoutes