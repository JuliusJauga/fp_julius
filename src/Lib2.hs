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
    
    -- Define the Name data type
    data Name = NumberName Int | WordName String | StringName String
        deriving (Show, Eq)

    -- Define the custom word type.
    newtype CustomWord = CustomWord String
        deriving (Show, Eq)
        
    -- Define the parser type
    type Parser a = String -> Either String (a, String)

    -- Define and
    and7' :: (a -> b -> c -> d -> e -> f -> g -> h) 
      -> Parser a 
      -> Parser b 
      -> Parser c 
      -> Parser d 
      -> Parser e 
      -> Parser f 
      -> Parser g 
      -> Parser h
    and7' g a b c d e f g' input =
        case a input of
            Right (v1, r1) -> 
                case b r1 of
                    Right (v2, r2) -> 
                        case c r2 of
                            Right (v3, r3) -> 
                                case d r3 of
                                    Right (v4, r4) -> 
                                        case e r4 of
                                            Right (v5, r5) -> 
                                                case f r5 of
                                                    Right (v6, r6) -> 
                                                        case g' r6 of
                                                            Right (v7, r7) -> Right (g v1 v2 v3 v4 v5 v6 v7, r7)
                                                            Left e7 -> Left e7
                                                    Left e6 -> Left e6
                                            Left e5 -> Left e5
                                    Left e4 -> Left e4
                            Left e3 -> Left e3
                    Left e2 -> Left e2
            Left e1 -> Left e1
    
    and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
    and6' g a b c d e f input =
        case a input of
            Right (v1, r1) ->
                case b r1 of
                    Right (v2, r2) ->
                        case c r2 of
                            Right (v3, r3) ->
                                case d r3 of
                                    Right (v4, r4) ->
                                        case e r4 of
                                            Right (v5, r5) ->
                                                case f r5 of
                                                    Right (v6, r6) -> Right (g v1 v2 v3 v4 v5 v6, r6)
                                                    Left e6 -> Left e6
                                            Left e5 -> Left e5
                                    Left e4 -> Left e4
                            Left e3 -> Left e3
                    Left e2 -> Left e2
            Left e1 -> Left e1

    and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
    and5' f a b c d e input =
        case a input of
            Right (v1, r1) ->
                case b r1 of
                    Right (v2, r2) ->
                        case c r2 of
                            Right (v3, r3) ->
                                case d r3 of
                                    Right (v4, r4) ->
                                        case e r4 of
                                            Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                                            Left e5 -> Left e5
                                    Left e4 -> Left e4
                            Left e3 -> Left e3
                    Left e2 -> Left e2
            Left e1 -> Left e1
    and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
    and4' f a b c d input =
        case a input of
            Right (v1, r1) ->
                case b r1 of
                    Right (v2, r2) ->
                        case c r2 of
                            Right (v3, r3) ->
                                case d r3 of
                                    Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                                    Left e4 -> Left e4
                            Left e3 -> Left e3
                    Left e2 -> Left e2
            Left e1 -> Left e1
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
    and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    and2' f a b input =
        case a input of
            Right (v1, r1) ->
                case b r1 of
                    Right (v2, r2) -> Right (f v1 v2, r2)
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
    -- Helper function to parse many elements using another parser
    parseMany :: Parser a -> Parser [a]
    parseMany parser input = 
        case parser input of
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

    -- Parse a stop
    -- <stop> ::= "(" <stop_id> ")"
    parseStop :: Parser Stop
    parseStop = and3' (\_ stopId _ -> Stop stopId) (char '(') name (char ')')

    -- Parse a list of stops
    -- <stop_list> ::= <stop>*
    parseStopList :: Parser [Stop]
    parseStopList = parseMany parseStop

    -- Parse a route
    -- <route> ::= "<" <route_id> "{" <stop_list> <nested_route_list> "}" ">"
    parseRoute :: Parser Route
    parseRoute = and7' makeRoute (char '<') name (char '{') parseStopList parseRouteList (char '}') (char '>')
        where makeRoute _ routeId _ stops'' nestedRoutes _ _ = Route routeId stops'' nestedRoutes

    -- Parse a list of routes
    -- <nested_route_list> ::= <route>*
    parseRouteList :: Parser [Route]
    parseRouteList input = 
        case parseMany parseRoute input of
            Right (routes', rest) -> Right (routes', rest)
            Left _ -> Right ([], input)


    -- Parse a route system
    -- <route_list_internal> ::= <route>*
    parseRouteSystem :: Parser [Route]
    parseRouteSystem = and3' (\_ routes' _ -> routes') (char '[') (parseMany parseRoute) (char ']')

    -- Helper function to parse a string. Just to parse querys
    string :: String -> Parser String
    string str input
        | str `isPrefixOf` input = Right (str, drop (length str) input)
        | otherwise = Left $ "Expected '" ++ str ++ "'"

    -- Parse a list-create query
    -- <list_create> ::= "list-create " <name>
    parseListCreate :: Parser Query
    parseListCreate = and2' (\_ listName -> ListCreate listName) (string "list-create ") name

    -- Parse a list-add query
    -- <list_add> ::= "list-add " <name> <route>
    parseListAdd :: Parser Query
    parseListAdd = and4' (\_ listName _ route -> ListAdd listName route) (string "list-add ") name (char ' ') parseRoute

    -- Parse a list-get query
    -- <list_get> ::= "list-get " <name>
    parseListGet :: Parser Query
    parseListGet = and2' (\_ listName -> ListGet listName) (string "list-get ") name

    -- Parse a list-remove query
    -- <list_remove> ::= "list-remove " <name>
    parseListRemove :: Parser Query
    parseListRemove = and2' (\_ listName -> ListRemove listName) (string "list-remove ") name

    -- Parse a list-remove query
    -- <list_remove> ::= "list-remove " <name>
    parseRouteCreate :: Parser Query
    parseRouteCreate = and2' (\_ routeId -> RouteCreate routeId) (string "route-create ") name
    
    -- Parse a route-get query
    -- <route_get> ::= "route-get " <name>
    parseRouteGet :: Parser Query
    parseRouteGet = and2' (\_ routeId -> RouteGet routeId) (string "route-get ") name
    
    -- Parse a route-add-route query
    -- <route_add_route> ::= "route-add-route " <route> <route>
    parseRouteAddRoute :: Parser Query
    parseRouteAddRoute = and4' 
        (\_ parentRoute _ childRoute -> RouteAddRoute parentRoute childRoute) 
        (string "route-add-route ") 
        parseRoute
        (char ' ')
        parseRoute
    -- Parse a route-remove query
    -- <route_remove> ::= "route-remove " <name>
    parseRouteRemove :: Parser Query
    parseRouteRemove = and2' (\_ routeId -> RouteRemove routeId) (string "route-remove ") name
    
    -- Parse a stop-create query
    -- <stop_create> ::= "stop-create " <name>
    parseStopCreate :: Parser Query
    parseStopCreate = and2' (\_ stopId -> StopCreate stopId) (string "stop-create ") name

    -- Parse a stop-delete query
    -- <stop_delete> ::= "stop-delete " <name>
    parseStopDelete :: Parser Query
    parseStopDelete = and2' (\_ stopId -> StopDelete stopId) (string "stop-delete ") name

    -- Parse a route-remove query
    -- <route_remove_stop> :: "route-remove-stop " <name> <stop>
    parseRouteRemoveStop :: Parser Query
    parseRouteRemoveStop = and4' (\_ routeId _ stop -> RouteRemoveStop routeId stop) (string "route-remove-stop ") name (char ' ') parseStop

    -- Parse a route-add query
    -- <route_add_stop> :: "route-add-stop " <name> <stop>
    parseRouteAddStop :: Parser Query
    parseRouteAddStop = and4' (\_ routeId _ stop -> RouteAddStop routeId stop) (string "route-add-stop ") name (char ' ') parseStop
    
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
    stateTransition :: State -> Query -> Either String State
    stateTransition (State routeLists routes' stops'') (ListCreate listName) =
        Right $ State ((listName, []) : routeLists) routes' stops''

    stateTransition (State routeLists routes' stops'') (ListAdd listName route) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId' route) routes'
            updatedRoute = route { stops' = stops' route }
        in case existingRoutes of
            [] -> Left "No existing routes found to add."
            (_:_) ->
                let routeTree = insertRoute updatedRoute EmptyTree
                    updatedRouteLists = map (\(name', trees) ->
                        if name' == listName
                        then (name', routeTree : trees)
                        else (name', trees)
                        ) routeLists
                in Right $ State updatedRouteLists remainingRoutes stops''

    stateTransition (State routeLists routes' stops'') (ListGet _) = 
        Right $ State routeLists routes' stops''

    stateTransition (State routeLists routes' stops'') (ListRemove listName) =
        let updatedRouteLists = filter (\(name', _) -> name' /= listName) routeLists
            removedRoutes = concatMap (\(_, trees) -> map rebuildRoute trees) (filter (\(name', _) -> name' == listName) routeLists)
            uniqueRemovedRoutes = filter (\r -> not (any (\r' -> routeId' r == routeId' r') routes')) removedRoutes
        in Right $ State updatedRouteLists (routes' ++ uniqueRemovedRoutes) stops''

    stateTransition (State routeLists routes' stops'') (RouteCreate routeId) =
        Right $ State routeLists (Route routeId [] [] : routes') stops''

    stateTransition (State routeLists routes' stops'') (RouteGet _) = 
        Right $ State routeLists routes' stops''

    stateTransition (State routeLists routes' stops'') (RouteAddRoute parentRoute childRoute) =
        let remainingRoutes = filter (\r -> routeId' r /= routeId' childRoute) routes'
            updateNestedRoutes route =
                if routeId' route == routeId' parentRoute
                then route { nestedRoutes' = nestedRoutes' route ++ [childRoute] }
                else route { nestedRoutes' = map updateNestedRoutes (nestedRoutes' route) }
            updatedRoutes = map updateNestedRoutes remainingRoutes
        in Right $ State routeLists updatedRoutes stops''

    stateTransition (State routeLists routes' stops'') (RouteRemove routeId) =
        let (removedRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case removedRoutes of
            [] -> Left "No route found to remove."
            (_:_) -> Right $ State routeLists remainingRoutes stops''

    stateTransition (State routeLists routes' stops'') (RouteAddStop routeId stop) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> Left "No route found to add a stop to."
            (r:_) ->
                let
                    updatedRoutes = map (\r' ->
                            if routeId' r' == routeId
                            then r' { stops' = stops' r' ++ [stop] }
                            else r'
                        ) (r : remainingRoutes)
                    updatedStops = filter (\s -> stopId' s /= stopId' stop) stops''
                in Right $ State routeLists updatedRoutes updatedStops

    stateTransition (State routeLists routes' stops'') (RouteRemoveStop routeId stop) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> Left "No route found to remove a stop from."
            (r:_) ->
                if stop `elem` stops' r
                then
                    let updatedRoutes = map (\r' ->
                            if routeId' r' == routeId
                            then r' { stops' = filter (\s -> stopId' s /= stopId' stop) (stops' r') }
                            else r'
                            ) (r : remainingRoutes)
                        updatedStops = stop : stops''
                    in Right $ State routeLists updatedRoutes updatedStops
                else Left "Stop not found in the specified route."

    stateTransition (State routeLists routes' stops'') (StopCreate stopId) = 
        Right $ State routeLists routes' (Stop stopId : stops'')

    stateTransition (State routeLists routes' stops'') (StopDelete stopId) =
        let (removedStops, remainingStops) = partition (\s -> stopId' s == stopId) stops''
        in case removedStops of
            [] -> Left "Stop not found to delete."
            (_:_) -> Right $ State routeLists routes' remainingStops


    -- Helper function to extract all stops from a RouteTree
    nodeStopsFromTree :: RouteTree -> [Stop]
    nodeStopsFromTree EmptyTree = []
    nodeStopsFromTree (Node nodeRoute childRoutes) =
        nodeStops nodeRoute ++ concatMap nodeStopsFromTree childRoutes