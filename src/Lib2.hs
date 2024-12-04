module Lib2 (
        Stop(..),
        Route(..),
        Query(..),
        State(..),
        RouteTree(..),
        NodeRoute(..),
        Name(..),
        Parser,
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
        parseRoute,
        and3',
        string,
        parseMany,
        and2',
        char,
        or2,
        routeTreeToRoute
    ) where

    import qualified Control.Monad.Trans.State.Strict as S (State, get, put, runState)
    import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
    import Control.Monad.Trans.Class (lift)
    import Data.Char (isAlphaNum, isDigit, isLetter)
    import Data.List (isPrefixOf, partition)

    -- Define the Name data type
    data Name = StringName String
        deriving (Show, Eq)

    -- Define the custom word type.
    newtype CustomWord = CustomWord String
        deriving (Show, Eq)

    -- Define the parser type
    type Parser a = ExceptT String (S.State String) a

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
    and7' f a b c d e f g h = do
        v1 <- a
        v2 <- b
        v3 <- c
        v4 <- d
        v5 <- e
        v6 <- f
        v7 <- g
        return $ f v1 v2 v3 v4 v5 v6 v7 h
        
    and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
    and6' f a b c d e g = do
        v1 <- a
        v2 <- b
        v3 <- c
        v4 <- d
        v5 <- e
        v6 <- g
        return $ f v1 v2 v3 v4 v5 v6

    and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
    and5' f a b c d e = do
        v1 <- a
        v2 <- b
        v3 <- c
        v4 <- d
        v5 <- e
        return $ f v1 v2 v3 v4 v5
        
    and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
    and4' f a b c d = do
        v1 <- a
        v2 <- b
        v3 <- c
        v4 <- d
        return $ f v1 v2 v3 v4
        
    and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
    and3' f a b c = do
        v1 <- a
        v2 <- b
        v3 <- c
        return $ f v1 v2 v3
    and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    and2' f a b = do
        v1 <- a
        v2 <- b
        return $ f v1 v2

    or2 :: Parser a -> Parser a -> Parser a
    or2 a b = do
        input <- lift S.get
        resultA <- runExceptT a
        case resultA of
            Right r1 -> return r1
            Left e1 -> do
                resultB <- runExceptT b
                case resultB of
                    Right r2 -> return r2
                    Left e2 -> throwE (e1 ++ ", " ++ e2)

    -- <char>
    parseChar :: Char -> Parser Char
    parseChar c = do
        input <- lift S.get
        case input of
            [] -> throwE $ "Cannot find " ++ [c] ++ " in an empty input"
            (h:t) -> if c == h then lift (S.put t) >> return c else throwE $ c : " is not found in " ++ [h]

    -- <word>
    parseWord :: Parser CustomWord
    parseWord = do
        input <- lift S.get
        let word = takeWhile isLetter input
            rest = drop (length word) input
        if null word
            then throwE "not a word"
            else lift (S.put rest) >> return (CustomWord word)

    -- <number>
    parseNumber :: Parser Int
    parseNumber = do
        input <- lift S.get
        let digits = takeWhile isDigit input
            rest = drop (length digits) input
        if null digits
            then throwE "not a number"
            else lift (S.put rest) >> return (read digits)

    -- -- <word> " " <word>
    -- parseWordName :: Parser Name
    -- parseWordName = and3' (\(CustomWord a) _ (CustomWord b) -> WordName (a ++ " " ++ b)) parseWord (parseChar ' ') parseWord

    -- parseNumberName :: Parser Name
    -- parseNumberName input =
    --     case parseNumber input of
    --         Right (num, rest) -> Right (NumberName num, rest)
    --         Left err -> Left err

    -- <string>
    parseStringName :: Parser Name
    parseStringName = do
        input <- lift S.get
        let str = takeWhile (/= ' ') input
            rest = drop (length str) input
        if null str
            then throwE "not a string"
            else lift (S.put rest) >> return (StringName str)

    -- <name> ::= <word> " " <word> | <number> | <string>
    parseName :: Parser Name
    parseName = parseStringName

    -- Parsing components
    -- Helper function to parse many elements using another parser
    parseMany :: Parser a -> Parser [a]
    parseMany parser = do
        input <- lift S.get
        case runExceptT parser input of
            Right (r, rest) -> do
                lift (S.put rest)
                rs <- parseMany parser
                return (r : rs)
            Left _ -> return []

    -- Parse a single character
    -- <char> ::= "a" | "b" | "c" ...
    char :: Char -> Parser Char
    char c = parseChar c

    -- Parse a stop
    -- <stop> ::= "(" <stop_id> ")"
    parseStop :: Parser Stop
    parseStop = and3' (\_ stopId _ -> Stop stopId) (char '(') parseName (char ')')

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
    string str = do
        input <- lift S.get
        if str `isPrefixOf` input
            then lift (S.put (drop (length str) input)) >> return str
            else throwE $ "Expected " ++ str ++ " but got " ++ input

    -- Parse a list-create query
    -- <list_create> ::= "list-create " <name>
    parseListCreate :: Parser Query
    parseListCreate = and2' (\_ listName -> ListCreate listName) (string "list-create ") name

    -- Parse a list-add query
    -- <list_add> ::= "list-add " <name> <route>
    parseListAdd :: Parser Query
    parseListAdd = and4' (\_ listName _ routeName -> ListAdd listName routeName) (string "list-add ") name (char ' ') name

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
    parseRouteCreate = and2' (\_ route -> RouteCreate route) (string "route-create ") parseRoute

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
        name
        (char ' ')
        name
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
    parseRouteRemoveStop = and4' (\_ routeId _ stop -> RouteRemoveStop routeId stop) (string "route-remove-stop ") name (char ' ') name

    -- Parse a route-add query
    -- <route_add_stop> :: "route-add-stop " <name> <stop>
    parseRouteAddStop :: Parser Query
    parseRouteAddStop = and4' (\_ routeId _ stop -> RouteAddStop routeId stop) (string "route-add-stop ") name (char ' ') name

    parseRoutesFromStop :: Parser Query
    parseRoutesFromStop = and2' (\_ stop -> RoutesFromStop stop) (string "routes-from-stop ") name

    -- Main query parser
    parseQuery :: Parser Query
    parseQuery =
        parseListAdd `or2` parseListCreate `or2` parseListGet `or2` parseListRemove `or2`
        parseRouteCreate `or2` parseRouteGet `or2` parseRouteAddRoute `or2` parseRouteRemove `or2`
        parseRouteAddStop `or2` parseRouteRemoveStop `or2` parseStopCreate `or2` parseStopDelete `or2` parseRoutesFromStop


    -- Query definition.
    data Query
        = ListCreate Name
        | ListAdd Name Name
        | ListGet Name
        | ListRemove Name
        | RouteCreate Route
        | RouteGet Name
        | RouteAddRoute Name Name
        | RouteAddStop Name Name
        | RouteRemoveStop Name Name
        | RouteRemove Name
        | StopCreate Name
        | StopDelete Name
        | RoutesFromStop Name
        deriving (Show, Eq)

    -- Define the State data type
    data State = State
        { routeTreeLists :: [(Name, [RouteTree])],
            routes :: [Route],
            stops :: [Stop]
        } deriving (Show, Eq)

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

    stateTransition (State routeLists routes' stops'') (ListAdd listName routeName) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeName) routes'
        in case existingRoutes of
            [] -> Left "No existing routes found to add."
            (_:_) ->
                let updatedRoute = head existingRoutes
                    routeTree = insertRoute updatedRoute EmptyTree
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

    stateTransition (State routeLists routes' stops'') (RouteCreate route) =
        if any (\r -> routeId' r == routeId' route) routes'
        then Left "Route already exists."
        else
            let stopIds = map stopId' (stops' route)
                allStopIds = map stopId' stops''
            in if all (`elem` allStopIds) stopIds
                then Right $ State routeLists (route : routes') stops''
                else Left "Stops don't exist in the stops list."

    stateTransition (State routeLists routes' stops'') (RouteGet _) =
        Right $ State routeLists routes' stops''

    stateTransition (State routeLists routes' stops'') (RouteAddRoute parentRouteName childRouteName) =
        let (parentRoutes, remainingRoutes) = partition (\r -> routeId' r == parentRouteName) routes'
            (childRoutes, remainingRoutes') = partition (\r -> routeId' r == childRouteName) remainingRoutes
            updateNestedRoutes route childRoute =
                route { 
                    stops' = stops' route ++ stops' childRoute,
                    nestedRoutes' = nestedRoutes' route ++ [childRoute]
                    }
        in case (parentRoutes, childRoutes) of 
            ([], _) -> Left "No parent route found."
            (_, []) -> Left "No child route found."
            (_:_, _:_) ->
                let updatedRoutes = map (\r ->
                        if routeId' r == parentRouteName
                        then updateNestedRoutes r (head childRoutes)
                        else r
                        ) (head parentRoutes : remainingRoutes')
                in Right $ State routeLists updatedRoutes stops''


    stateTransition (State routeLists routes' stops'') (RouteRemove routeId) =
        let (removedRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case removedRoutes of
            [] -> Left "No route found to remove."
            (_:_) -> Right $ State routeLists remainingRoutes stops''

    stateTransition (State routeLists routes' stops'') (RouteAddStop routeId stopName) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> Left "No route found to add a stop to."
            (r:_) ->
                if stopName `elem` map stopId' stops''
                then
                    let
                        updatedRoutes = map (\r' ->
                                if routeId' r' == routeId
                                then r' { stops' = stops' r' ++ [Stop stopName] }
                                else r'
                            ) (r : remainingRoutes)
                    in Right $ State routeLists updatedRoutes stops''
                else Left "Stop not found in the stops list."

    stateTransition (State routeLists routes' stops'') (RouteRemoveStop routeId stopName) =
        let (existingRoutes, remainingRoutes) = partition (\r -> routeId' r == routeId) routes'
        in case existingRoutes of
            [] -> Left "No route found to remove a stop from."
            (r:_) ->
                if stopName `elem` map stopId' (stops' r)
                then
                    let updatedRoutes = map (\r' ->
                            if routeId' r' == routeId
                            then r' { stops' = filter (\s -> stopId' s /= stopName) (stops' r') }
                            else r'
                            ) (r : remainingRoutes)
                    in Right $ State routeLists updatedRoutes stops''
                else Left "Stop not found in the specified route."

    stateTransition (State routeLists routes' stops'') (StopCreate stopId) =
        if any (\s -> stopId' s == stopId) stops''
        then Left "Stop already exists."
        else Right $ State routeLists routes' (Stop stopId : stops'')

    stateTransition (State routeLists routes' stops'') (StopDelete stopId) =
        let (removedStops, remainingStops) = partition (\s -> stopId' s == stopId) stops''
        in case removedStops of
            [] -> Left "Stop not found to delete."
            (_:_) -> Right $ State routeLists routes' remainingStops
    stateTransition (State routeLists routes' _) (RoutesFromStop stopName) =
        let routesFromStop' = routesFromStopInLists (Stop stopName) routeLists
            routesFromStop'' = routeFromStopInNestedRoutes (Stop stopName) routes'
        in Left $ displayRoutesFromStop (routesFromStop' ++ routesFromStop'') (Stop stopName)
    
    routeTreeToRoute :: RouteTree -> Route
    routeTreeToRoute EmptyTree = Route singletonName [] []
    routeTreeToRoute (Node nodeRoute childRoutes) =
        Route (nodeRouteId nodeRoute) (nodeStops nodeRoute) (map routeTreeToRoute childRoutes)

    -- Helper function to extract all stops from a RouteTree
    nodeStopsFromTree :: RouteTree -> [Stop]
    nodeStopsFromTree EmptyTree = []
    nodeStopsFromTree (Node nodeRoute childRoutes) =
        nodeStops nodeRoute ++ concatMap nodeStopsFromTree childRoutes

    -- Helper function to extract all possible routes from a stop in a route tree
    -- This is a recursive function that traverses the tree and finds all possible routes from a stop
    -- It returns a list of routes that contain the stop
    routesFromStop :: Stop -> RouteTree -> [Route]
    routesFromStop _ EmptyTree = []
    routesFromStop stop (Node nodeRoute childRoutes) =
        let routesFromChildren = concatMap (routesFromStop stop) childRoutes
        in if stop `elem` nodeStops nodeRoute
            then Route (nodeRouteId nodeRoute) (nodeStops nodeRoute) [] : routesFromChildren
            else routesFromChildren

    routesFromStopInList :: Stop -> [RouteTree] -> [Route]
    routesFromStopInList stop = concatMap (routesFromStop stop)

    routesFromStopInLists :: Stop -> [(Name, [RouteTree])] -> [Route]
    routesFromStopInLists stop = concatMap (routesFromStopInList stop . snd)

    routeFromStopInNestedRoutes :: Stop -> [Route] -> [Route]
    routeFromStopInNestedRoutes _ [] = []
    routeFromStopInNestedRoutes stop routes' =
        let newTree = singletonRoute (Route singletonName [] [])
            newTreeInserted = foldl (flip insertRoute) newTree routes'
        in  routesFromStop stop newTreeInserted

    displayRoutesFromStop :: [Route] -> Stop -> String
    displayRoutesFromStop [] stop = "No routes found from " ++ displayStopName stop
    displayRoutesFromStop routes' stop = "Routes found from " ++ displayStopName stop ++ " are: " ++ concatMap displayRouteName routes'

    displayRouteName :: Route -> String
    displayRouteName route = show (routeId' route) ++ " "

    displayStopName :: Stop -> String
    displayStopName = show



    
