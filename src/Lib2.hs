module Lib2 (
        Stop(..),      
        Route(..),  
        Command(..),
        State(..),
        parseCommand,
        parseRouteSystem,
        stateTransition
    ) where
    
    import Data.Char (isSpace, isAlphaNum)
    import Data.List (isPrefixOf)

    -- Parsing components

    -- Helper function to parse many elements
    parseMany :: (String -> Either String (a, String)) -> String -> Either String ([a], String)
    parseMany parser input = case parser input of
        Right (x, rest) -> 
            case parseMany parser rest of
                Right (xs, rest') -> Right (x:xs, rest')
                Left _ -> Right ([x], rest)
        Left _ -> Right ([], input)
    
    -- Parse a single character
    char :: Char -> String -> Either String (Char, String)
    char c (x:xs)
        | c == x = Right (c, xs)
        | otherwise = Left $ "Expected '" ++ [c] ++ "'"
    char _ [] = Left "Unexpected end of input"

    -- Parse a name
    name :: String -> Either String (String, String)
    name input = 
        let (n, rest) = span (\c -> isAlphaNum c || c == ' ') input
            trimmedName = reverse (dropWhile isSpace (reverse (dropWhile isSpace n)))
        in if null trimmedName then Left "Expected a name, but found none"
        else Right (trimmedName, rest)

    -- Parse a stop
    parseStop :: String -> Either String (Stop, String)
    parseStop input =
        case char '(' input of
            Right (_, rest1) -> 
                case name rest1 of
                    Right (stopId', rest2) -> 
                        case char ')' rest2 of
                            Right (_, rest3) -> Right (Stop stopId', rest3)
                            Left _ -> Left "Expected ')' at the end of stop."
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected '(' at the start of stop."

    -- Parse a list of stops
    parseStopList :: String -> Either String ([Stop], String)
    parseStopList = parseMany parseStop

    -- Parse a route
    parseRoute :: String -> Either String (Route, String)
    parseRoute input =
        case char '<' input of
            Right (_, rest1) -> 
                case name rest1 of
                    Right (routeId', rest2) -> 
                        case char '{' rest2 of
                            Right (_, rest3) -> 
                                case parseStopList rest3 of
                                    Right (stops, rest4) -> 
                                        case char '}' rest4 of
                                            Right (_, rest5) -> 
                                                case char '>' rest5 of
                                                    Right (_, rest6) -> Right (Route routeId' stops [], rest6)
                                                    Left _ -> Right (Route routeId' stops [], rest5)
                                            Left _ -> Left "Expected '}' at the end of stops."
                                    Left _ -> Right (Route routeId' [] [], rest3)
                            Left _ -> Left "Expected '{' at the start of stops."
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected '<' at the start of route."

    -- Parse a list of routes
    parseRouteList :: String -> Either String ([Route], String)
    parseRouteList input =
        case char '[' input of
            Right (_, rest1) -> 
                case parseMany parseRoute rest1 of
                    Right (routes, rest2) -> 
                        case char ']' rest2 of
                            Right (_, rest3) -> Right (routes, rest3)
                            Left _ -> Left "Expected ']' at the end of route list."
                    Left _ -> Left "Failed to parse route list."
            Left _ -> Left "Expected '[' at the start of route list."

    -- Parse a route system
    parseRouteSystem :: String -> Either String ([Route], String)
    parseRouteSystem input =
        case parseRouteList input of
            Right (routes, rest) -> Right (routes, rest)
            Left _ -> Left "Failed to parse route system."

    -- Helper function to parse a string
    string :: String -> String -> Either String (String, String)
    string str input
        | str `isPrefixOf` input = Right (str, drop (length str) input)
        | otherwise = Left $ "Expected '" ++ str ++ "'"
    -- Parsing commands

    data Command
        = ListCreate String
        | ListAdd String Route
        | ListGet String
        | ListRemove String Route
        | RouteCreate String
        | RouteGet String
        | RouteAddRoute Route Route
        | RouteRemove String
        | StopCreate String
        | StopDelete String
        deriving (Show, Eq)

    -- Define the Route data type
    data Route = Route
        { routeId :: String
        , stops :: [Stop]
        , nestedRoutes :: [Route]
        } deriving (Show, Eq)

    -- Define the Stop data type
    data Stop = Stop
        { stopId :: String
        } deriving (Show, Eq)

    -- Parsing functions
    parseListCreate :: String -> Either String (Command, String)
    parseListCreate input =
        case string "list-create " input of
            Right (_, rest) -> 
                case name rest of
                    Right (listName, rest') -> Right (ListCreate listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-create'."

    parseListAdd :: String -> Either String (Command, String)
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

    parseListGet :: String -> Either String (Command, String)
    parseListGet input =
        case string "list-get " input of
            Right (_, rest) -> 
                case name rest of
                    Right (listName, rest') -> Right (ListGet listName, rest')
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-get'."

    parseListRemove :: String -> Either String (Command, String)
    parseListRemove input =
        case string "list-remove " input of
            Right (_, rest) -> 
                case name rest of
                    Right (listName, rest1) -> 
                        case parseRoute rest1 of
                            Right (route, rest2) -> Right (ListRemove listName route, rest2)
                            Left _ -> Left "Expected a valid route."
                    Left _ -> Left "Expected a valid list name."
            Left _ -> Left "Expected 'list-remove'."

    parseRouteCreate :: String -> Either String (Command, String)
    parseRouteCreate input =
        case string "route-create " input of
            Right (_, rest) -> 
                case name rest of
                    Right (routeId, rest') -> Right (RouteCreate routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-create'."

    parseRouteGet :: String -> Either String (Command, String)
    parseRouteGet input =
        case string "route-get " input of
            Right (_, rest) -> 
                case name rest of
                    Right (routeId, rest') -> Right (RouteGet routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-get'."

    parseRouteAddRoute :: String -> Either String (Command, String)
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

    parseRouteRemove :: String -> Either String (Command, String)
    parseRouteRemove input =
        case string "route-remove " input of
            Right (_, rest) -> 
                case name rest of
                    Right (routeId, rest') -> Right (RouteRemove routeId, rest')
                    Left _ -> Left "Expected a valid route ID."
            Left _ -> Left "Expected 'route-remove'."

    parseStopCreate :: String -> Either String (Command, String)
    parseStopCreate input =
        case string "stop-create " input of
            Right (_, rest) -> 
                case name rest of
                    Right (stopId, rest') -> Right (StopCreate stopId, rest')
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected 'stop-create'."

    parseStopDelete :: String -> Either String (Command, String)
    parseStopDelete input =
        case string "stop-delete " input of
            Right (_, rest) -> 
                case name rest of
                    Right (stopId, rest') -> Right (StopDelete stopId, rest')
                    Left _ -> Left "Expected a valid stop ID."
            Left _ -> Left "Expected 'stop-delete'."

    -- Main command parser
    parseCommand :: String -> Either String Command
    parseCommand input =
        case parseListCreate input of
            Right (command, _) -> Right command
            Left _ -> 
                case parseListAdd input of
                    Right (command, _) -> Right command
                    Left _ -> 
                        case parseListGet input of
                            Right (command, _) -> Right command
                            Left _ -> 
                                case parseListRemove input of
                                    Right (command, _) -> Right command
                                    Left _ -> 
                                        case parseRouteCreate input of
                                            Right (command, _) -> Right command
                                            Left _ -> 
                                                case parseRouteGet input of
                                                    Right (command, _) -> Right command
                                                    Left _ -> 
                                                        case parseRouteAddRoute input of
                                                            Right (command, _) -> Right command
                                                            Left _ -> 
                                                                case parseRouteRemove input of
                                                                    Right (command, _) -> Right command
                                                                    Left _ -> 
                                                                        case parseStopCreate input of
                                                                            Right (command, _) -> Right command
                                                                            Left _ -> 
                                                                                case parseStopDelete input of
                                                                                    Right (command, _) -> Right command
                                                                                    Left _ -> Left "Invalid command."
        
    -- State transitions
    newtype State = State
        { routes :: [Route]
        } deriving (Show, Eq)

    stateTransition :: State -> Command -> Either String ([String], State)
    stateTransition (State routes) (ListCreate listName) = 
        let newRoute = Route listName [] []
        in Right (["Created list"], State (newRoute : routes))
    
    stateTransition (State routes) (ListAdd listName route) = 
        let updatedRoutes = map (\r -> if routeId r == listName 
                                       then r { nestedRoutes = route : nestedRoutes r }
                                       else r) routes
        in if updatedRoutes == routes
            then Left "List not found."
            else Right (["Added route to list"], State updatedRoutes)
    
    stateTransition (State routes) (ListGet listName) = 
        let listRoutes = case filter (\r -> routeId r == listName) routes of
                            [r] -> nestedRoutes r
                            _ -> []
        in Right (map routeId listRoutes, State routes)
        
    stateTransition (State routes) (ListRemove listName route) = 
        let updatedRoutes = map (\r -> if routeId r == listName 
                                       then r { nestedRoutes = filter (\nr -> routeId nr /= routeId route) (nestedRoutes r) }
                                       else r) routes
        in if updatedRoutes == routes
            then Left "List or route not found."
            else Right (["Removed route from list"], State updatedRoutes)
    
    stateTransition (State routes) (RouteCreate routeId) =
        let newRoute = Route routeId [] []
        in Right (["Created route"], State (newRoute : routes))
    
    stateTransition (State routes) (RouteGet rid) =
        let route = case filter (\r -> routeId r == rid) routes of
                        [r] -> r
                        _ -> Route "" [] []
        in Right ([show route], State routes)
    
    stateTransition (State routes) (RouteAddRoute parentRoute childRoute) =
        let updatedRoutes = map (\r -> if routeId r == routeId parentRoute 
                                       then r { nestedRoutes = childRoute : nestedRoutes r }
                                       else r) routes
        in if updatedRoutes == routes
            then Left "Parent route not found."
            else Right (["Added route to parent route"], State updatedRoutes)
    
    stateTransition (State routes) (RouteRemove rid) =
        let updatedRoutes = filter (\r -> routeId r /= rid) routes
        in if updatedRoutes == routes
            then Left "Route not found."
            else Right (["Removed route"], State updatedRoutes)
    
    stateTransition (State routes) (StopCreate stopId) =
        let newStop = Stop stopId
        in Right (["Created stop"], State routes)
    
    stateTransition (State routes) (StopDelete sid) =
        let updatedRoutes = map (\r -> r { stops = filter (\s -> stopId s /= sid) (stops r) }) routes
        in Right (["Deleted stop"], State updatedRoutes)
    