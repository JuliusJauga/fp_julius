module Lib2 (
        Stop(..),
        Route(..),
        Query(..),
        State(..),
        RouteTree(..),
        NodeRoute(..),
        Name(..),
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
    ) where

    import Data.Char (isAlphaNum, isDigit, isLetter)
    import Data.List (isPrefixOf, partition)

    data Name = NumberName Int | WordName String | StringName String
        deriving (Show, Eq)

    newtype CustomWord = CustomWord String
        deriving (Show, Eq)
        
    type Parser a = String -> Either String (a, String)

    and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
    and3' f a b c input =
        case a input of
            Right (v1, r1) ->
                case b r1 of
                    Right (v2, r2) ->
                        case c r2 of
                            Right (v3, r3) -> Right (f v1 v2 v3, r3)
                            Left e3 -> Left e3
                    Left e2 -> Left e2
            Left e1 -> Left e1

            
    or2 :: Parser a -> Parser a -> Parser a
    or2 a b input =
        case a input of
            Right r1 -> Right r1
            Left e1 ->
                case b input of
                    Right r2 -> Right r2
                    Left e2 -> Left (e1 ++ ", " ++ e2)
    
    -- <char>
    parseChar :: Char -> Parser Char
    parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
    parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

    -- <word>
    parseWord :: Parser CustomWord
    parseWord input =
        let letters = takeWhile isLetter input
            rest = drop (length letters) input
        in if not (null letters)
            then Right (CustomWord letters, rest)
            else Left (input ++ " does not start with a letter")

    -- <number>
    parseNumber :: Parser Int
    parseNumber [] = Left "empty input, cannot parse a number"
    parseNumber str =
        let
            digits = takeWhile isDigit str
            rest = drop (length digits) str
        in
            case digits of
                [] -> Left "not a number"
                _ -> Right (read digits, rest)

    -- <word> " " <word>
    parseWordName :: Parser Name
    parseWordName = and3' (\(CustomWord a) _ (CustomWord b) -> WordName (a ++ " " ++ b)) parseWord (parseChar ' ') parseWord

    parseNumberName :: Parser Name
    parseNumberName input = 
        case parseNumber input of
            Right (num, rest) -> Right (NumberName num, rest)
            Left err -> Left err
    
    -- <string>
    parseStringName :: Parser Name
    parseStringName input =
        let (strName, rest) = span (\c -> isAlphaNum c || c == '_') input
        in if null strName
            then Left "Expected a valid string name."
            else Right (StringName strName, rest)

    -- <name> ::= <word> " " <word> | <number> | <string>
    name :: Parser Name
    name = parseWordName `or2` parseNumberName `or2` parseStringName

    -- Parsing components

    -- Helper to aggregate error messages with a focus on route parsing
    orElse :: (String -> Either String a) -> (String -> Either String a) -> String -> Either String a
    orElse p1 p2 input = case p1 input of
        Right result -> Right result
        Left err1 -> case p2 input of
            Right result -> Right result
            Left err2
                -- Check if both errors are about invalid route format
                | err1 == "Invalid route format." && err2 == "Invalid route format." ->
                    Left "Invalid route format. Expected a valid route definition."
                -- Avoid repeating the combined error message
                | "Invalid route format. Expected a valid route definition." `isPrefixOf` err1 -> Left err1
                | "Invalid route format. Expected a valid route definition." `isPrefixOf` err2 -> Left err2
                | otherwise -> Left (err1 ++ "; " ++ err2)



    -- Helper function to parse many elements using another parser
    -- type Parser a = String -> Either String (a, String)
    parseMany :: Parser a -> Parser [a]
    parseMany parser input = case parser input of
        Right (x, rest) ->
            case parseMany parser rest of
                Right (xs, rest') -> Right (x:xs, rest')
                Left _ -> Right ([x], rest)
        Left _ -> Right ([], input)

    -- Parse a single character
    -- <char> ::= "a" | "b" | "c" ...
    char :: Char -> Parser Char
    char c (x:xs)
        | c == x = Right (c, xs)
        | otherwise = Left $ "Expected '" ++ [c] ++ "'"
    char _ [] = Left "Unexpected end of input"

    -- Parse a name
    -- <name> ::= <char>+ | <name> " " <name>
    -- name :: String -> Either String (String, String)
    -- name input =
    --     let (n, rest) = span (\c -> isAlphaNum c || c == ' ') input
    --         trimmedName = reverse (dropWhile isSpace (reverse (dropWhile isSpace n)))
    --     in if null trimmedName then Left "Expected a name, but found none"
    --     else Right (trimmedName, rest)

    -- Parse a stop
    -- <stop> ::= "(" <stop_id> ")"
    parseStop :: Parser Stop
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
    parseStopList :: Parser [Stop]
    parseStopList = parseMany parseStop

    -- Parse a route
    -- <route> ::= "<" <route_id> "{" <stop_list> <nested_route_list> "}" ">"
    parseRoute :: Parser Route
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
    -- <nested_route_list> ::= <route>*
    parseRouteList :: Parser [Route]
    parseRouteList input =
            case parseMany parseRoute input of
                Right (routes'', rest1) -> Right (routes'', rest1)
                Left _ -> Left "Failed to parse route list."

    -- Parse a route system
    -- <route_list_internal> ::= <route>*
    parseRouteSystem :: Parser [Route]
    parseRouteSystem input =
        case char '[' input of
            Right (_, rest1) ->
                case parseMany parseRoute rest1 of
                    Right (routes'', rest2) ->
                        case char ']' rest2 of
                            Right (_, rest3) -> Right (routes'', rest3)
                            Left _ -> Left "Failed to parse route system."
                    Left _ -> Left "Failed to parse route list."
            Left _ -> Left "Expected '[' at the start of route system."

    -- Helper function to parse a string. Just to parse querys
    string :: String -> Parser String
    string str input
        | str `isPrefixOf` input = Right (str, drop (length str) input)
        | otherwise = Left $ "Expected '" ++ str ++ "'"



    -- Parsing functions

    -- Parse a list-create query
    -- <list_create> ::= "list-create " <name>
    parseListCreate :: Parser Query
    parseListCreate input =
        case string "list-create " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest') -> Right (ListCreate listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-create'."

    -- Parse a list-add query
    -- <list_add> ::= "list-add " <name> <route>
    parseListAdd :: Parser Query
    parseListAdd input =
        case string "list-add " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest1) ->
                        case char ' ' rest1 of
                            Right (_, rest2) ->
                                case parseRoute rest2 of
                                    Right (route, rest3) -> Right (ListAdd listName route, rest3)
                                    Left _ -> Left "Expected a valid route."
                            Left _ -> Left "Expected whitespace after list name."
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-add'."

    -- Parse a list-get query
    -- <list_get> ::= "list-get " <name>
    parseListGet :: Parser Query
    parseListGet input =
        case string "list-get " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest') -> Right (ListGet listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-get'."
    
    -- Parse a list-remove query
    -- <list_remove> ::= "list-remove " <name>
    parseListRemove :: Parser Query
    parseListRemove input =
        case string "list-remove " input of
            Right (_, rest) ->
                case name rest of
                    Right (listName, rest') -> Right (ListRemove listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-remove'."

    -- Parse a list-remove query
    -- <list_remove> ::= "list-remove " <name>
    parseRouteCreate :: Parser Query
    parseRouteCreate input =
        case string "route-create " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest') -> Right (RouteCreate routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-create'."

    -- Parse a route-get query
    -- <route_get> ::= "route-get " <name>
    parseRouteGet :: Parser Query
    parseRouteGet input =
        case string "route-get " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest') -> Right (RouteGet routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-get'."

    -- Parse a route-add-route query
    -- <route_add_route> ::= "route-add-route " <route> <route>
    parseRouteAddRoute :: Parser Query
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
    parseRouteRemove :: Parser Query
    parseRouteRemove input =
        case string "route-remove " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest') -> Right (RouteRemove routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-remove'."

    -- Parse a stop-create query
    -- <stop_create> ::= "stop-create " <name>
    parseStopCreate :: Parser Query
    parseStopCreate input =
        case string "stop-create " input of
            Right (_, rest) ->
                case name rest of
                    Right (stopId, rest') -> Right (StopCreate stopId, rest')
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected 'stop-create'."

    -- Parse a stop-delete query
    -- <stop_delete> ::= "stop-delete " <name>
    parseStopDelete :: Parser Query
    parseStopDelete input =
        case string "stop-delete " input of
            Right (_, rest) ->
                case name rest of
                    Right (stopId, rest') -> Right (StopDelete stopId, rest')
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected 'stop-delete'."

    -- Parse a route-remove query
    -- <route_remove_stop> :: "route-remove-stop " <name> <stop>
    parseRouteRemoveStop :: Parser Query
    parseRouteRemoveStop input = 
        case string "route-remove-stop " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest1) ->
                        case char ' ' rest1 of
                            Right (_, rest2) ->
                                case parseStop rest2 of
                                    Right (stop, rest3) -> Right (RouteRemoveStop routeId stop, rest3)
                                    Left _ -> Left "Expected a valid stop."
                            Left _ -> Left "Expected whitespace after route ID."
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-remove-stop'."

    -- Parse a route-add query
    -- <route_add_stop> :: "route-add-stop " <name> <stop>
    parseRouteAddStop :: Parser Query
    parseRouteAddStop input =
        case string "route-add-stop " input of
            Right (_, rest) ->
                case name rest of
                    Right (routeId, rest1) ->
                        case char ' ' rest1 of
                            Right (_, rest2) ->
                                case parseStop rest2 of
                                    Right (stop, rest3) -> Right (RouteAddStop routeId stop, rest3)
                                    Left _ -> Left "Expected a valid stop."
                            Left _ -> Left "Expected whitespace after route ID."
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-add-stop'."

    -- Main query parser
    parseQuery :: Parser Query
    parseQuery = 
        parseListAdd `or2` parseListCreate `or2` parseListGet `or2` parseListRemove `or2`
        parseRouteCreate `or2` parseRouteGet `or2` parseRouteAddRoute `or2` parseRouteRemove `or2`
        parseRouteAddStop `or2` parseRouteRemoveStop `or2` parseStopCreate `or2` parseStopDelete

    -- Query definition.
    data Query
        = ListCreate Name
        | ListAdd Name Route
        | ListGet Name
        | ListRemove Name
        | RouteCreate Name
        | RouteGet Name
        | RouteAddRoute Route Route
        | RouteAddStop Name Stop
        | RouteRemoveStop Name Stop
        | RouteRemove Name
        | StopCreate Name
        | StopDelete Name
        deriving (Show, Eq)

    -- Define the Route data type
    data Route = Route
        { routeId' :: Name
        , stops' :: [Stop]
        , nestedRoutes' :: [Route]
        } deriving (Show, Eq)

    -- Define the Stop data type
    newtype Stop = Stop
        { stopId' :: Name
        } deriving (Show, Eq)

    -- Define the RouteTree data type
        -- This is a tree structure that represents a route system
        -- Every node in the tree is a Route, and child nodes are nested routes
    
    data RouteTree = EmptyTree | Node NodeRoute [RouteTree]
        deriving (Show, Eq)

    -- Define the NodeRoute data type
    data NodeRoute = NodeRoute
        { nodeRouteId :: Name
        , nodeStops :: [Stop]
        } deriving (Show, Eq)

    -- Helper functio to make a new RouteTree
    singletonRoute :: Route -> RouteTree
    singletonRoute route = Node (NodeRoute (routeId' route) (stops' route)) []

    -- Inserting a route to a tree, this is for building a tree structure from route.
    insertRoute :: Route -> RouteTree -> RouteTree
    insertRoute route EmptyTree =
        let newNode = singletonRoute route
            newNodeWithChildren = insertNestedRoutes newNode (nestedRoutes' route)
        in newNodeWithChildren
    insertRoute route (Node rootRoute childRoutes) =
        let newNode = singletonRoute route
            newNodeWithChildren = insertNestedRoutes newNode (nestedRoutes' route)
        in Node rootRoute (newNodeWithChildren : childRoutes)

    -- -- Helper function to insert nested routes into a RouteTree 
    insertNestedRoutes :: RouteTree -> [Route] -> RouteTree
    insertNestedRoutes = foldl (flip insertRoute)

    -- Helper function to rebuild a Route from a RouteTree
    rebuildRoute :: RouteTree -> Route
    rebuildRoute EmptyTree = Route singletonName [] []
    rebuildRoute (Node nodeRoute subTrees) =
        Route (nodeRouteId nodeRoute) (nodeStops nodeRoute) (map rebuildRoute subTrees)

    singletonName :: Name
    singletonName = StringName ""

    -- Define the State data type
    data State = State
        { routeTreeLists :: [(Name, [RouteTree])],
            routes :: [Route],
            stops :: [Stop]
        } deriving (Show, Eq)

    -- Define initial state
    initialState :: State
    initialState = State [] [] []

    -- Define state transitions, this works with Tree data structure of RouteTree
    -- RouteTreeLists are separate from routes and stops lists.
    -- When adding a route to RouteTreeList, it adds a new RouteTree to that list
    -- All of the nested routes get built in tree structure. Nested routes become child Nodes of a root route.
    -- When removing a list, the same tree structures get rebuilt back into Route types recursively.
    stateTransition :: State -> Query -> State
    stateTransition (State routeLists routes' stops'') (ListCreate listName) =
        State ((listName, []) : routeLists) routes' stops''
    stateTransition (State routeLists routes' stops'') (ListAdd listName route) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId' route) routes'
            updatedRoute = route { stops' = stops' route }
        in case existingRoutes of
            [] -> State routeLists routes' stops''
            (_:_) ->
                let routeTree = insertRoute updatedRoute EmptyTree
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
            uniqueRemovedRoutes = filter (\r -> not (any (\r' -> routeId' r == routeId' r') routes')) removedRoutes
        in State updatedRouteLists (routes' ++ uniqueRemovedRoutes) stops''
    stateTransition (State routeLists routes' stops'') (RouteCreate routeId) =
        State routeLists (Route routeId [] [] : routes') stops''
    stateTransition (State routeLists routes' stops'') (RouteGet _) = State routeLists routes' stops''
    stateTransition (State routeLists routes' stops'') (RouteAddRoute parentRoute childRoute) =
        let
            remainingRoutes = filter (\r -> routeId' r /= routeId' childRoute) routes'
            updateNestedRoutes route =
                if routeId' route == routeId' parentRoute
                then route { nestedRoutes' = nestedRoutes' route ++ [childRoute] }
                else route { nestedRoutes' = map updateNestedRoutes (nestedRoutes' route) }
            updatedRoutes = map updateNestedRoutes remainingRoutes
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
            (r:_) ->
                let updatedRoutes = map (\r' ->
                        if routeId' r' == routeId
                        then r' { stops' = stops' r' ++ [stop] }
                        else r'
                        ) (r : remainingRoutes)
                in State routeLists updatedRoutes stops''
    stateTransition (State routeLists routes' stops'') (RouteRemoveStop routeId stop) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> State routeLists routes' stops''
            (r:_) ->
                if stop `elem` stops' r
                then
                    let updatedRoutes = map (\r' ->
                            if routeId' r' == routeId
                            then r' { stops' = filter (\s -> stopId' s /= stopId' stop) (stops' r') }
                            else r'
                            ) (r : remainingRoutes)
                        updatedStops = stop : stops''
                    in State routeLists updatedRoutes updatedStops
                else State routeLists routes' stops''
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