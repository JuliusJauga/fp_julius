{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements (..),
    Command (..),
    ) where

import Control.Concurrent ( Chan , readChan, writeChan, newChan )
import Control.Concurrent.STM(TVar, atomically, readTVarIO, readTVar, writeTVar)
import Data.List (isPrefixOf, isSuffixOf, notElem, (\\), intercalate, find, delete, partition)
import qualified Lib2
import Data.Char (isSpace)
import Control.Exception (try, SomeException)
import Debug.Trace
import Data.Either (partitionEithers)

-- instance Show Lib2.Query where
--     show = renQuery 

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  operation <- readChan chan
  case operation of
    Save content notifyChan -> do
      result <- try $ writeFile "saved.txt" content
      case result of
        Left err -> putStrLn $ "Error saving state" ++ show (err :: SomeException)
        Right _  -> writeChan notifyChan ()
      storageOpLoop chan

    Load notifyChan -> do
      result <- try $ readFile "saved.txt"
      case result of
        Left err      -> writeChan notifyChan $ "Error loading state: " ++ show (err :: SomeException)
        Right content -> writeChan notifyChan content
      storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Helper function
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (\c -> c == ' ' || c == '\n')


-- | Parses user's input.
parseCommand :: Lib2.Parser Command
parseCommand input
    | "load" `isPrefixOf` input = Right (LoadCommand, drop 4 input)
    | "save" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
    | "BEGIN" `isPrefixOf` input = case parseStatements input of
        Left err -> Left err
        Right (stmts, rest) -> Right (StatementCommand stmts, rest)
    | otherwise = case Lib2.parseQuery (trim input) of
        Left err -> Left err
        Right (query, rest) -> Right (StatementCommand (Single query), rest)
        
-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: Lib2.Parser Statements
parseStatements input =
    let inputTrimmed = trim input
    in if "BEGIN" `isPrefixOf` inputTrimmed
        then
            if "END" `isSuffixOf` inputTrimmed
                then
                    let body = trim $ drop 5 $ take (length inputTrimmed - 3) inputTrimmed
                    in if null body
                        then Right (Batch [], "") -- empty batch
                        else
                            let queries = map (Lib2.parseQuery . trim) (filter (not . null) $ splitOn ';' body)
                                (errors, parsedQueries) = partitionEithers queries
                            in if null errors
                                then
                                    let queriesList = map fst parsedQueries
                                    in if length queriesList == 1
                                        then Right (Single (head queriesList), "")
                                        else Right (Batch queriesList, "")
                                else Left $ "Error parsing queries: " ++ unlines errors
                else Left "Batch must end with 'END'"
        else Left "Batch must start with 'BEGIN'"

-- parseStatements :: String -> Either String (Statements, String)
-- parseStatements input =
--     case batchParser (trim input) of
--         Right (queries, rest) -> Right (Batch queries, rest)
--         Left _ ->
--             case commandParser (trim input) of
--                 Right (query, rest) -> Right (Single query, rest)
--                 Left err -> Left err

-- -- Parser for <batch>
-- batchParser :: Lib2.Parser [Lib2.Query]
-- batchParser = Lib2.and3' (\_ cmds _ -> cmds) (Lib2.string "BEGIN") commandsListParser (Lib2.string "END")

-- -- Parser for <commands_list>
-- commandsListParser :: Lib2.Parser [Lib2.Query]
-- commandsListParser = Lib2.parseMany commandParser

-- -- Parser for <commands>
-- commandParser :: Lib2.Parser Lib2.Query
-- commandParser = Lib2.and2' (\query _ -> query) Lib2.parseQuery (Lib2.char ';')


splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
  | c == x = "" : rest
  | otherwise = (x : head rest) : tail rest
  where rest = splitOn c xs

marshallState :: Lib2.State -> Statements
marshallState (Lib2.State routeTreeLists routes stops) =
    let stopQueries = map stopToQuery stops
        routeQueries = concatMap routeToQueries routes
        listQueries = concatMap listToQueries routeTreeLists
    in Batch (stopQueries ++ routeQueries ++ listQueries)

-- Converts a Stop into a StopCreate query
stopToQuery :: Lib2.Stop -> Lib2.Query
stopToQuery (Lib2.Stop stopId) = Lib2.StopCreate stopId

-- Converts a Route into a series of queries, including RouteAddStop queries
routeToQueries :: Lib2.Route -> [Lib2.Query]
routeToQueries (Lib2.Route routeId routeStops nestedRoutes) =
    let createRouteQuery = Lib2.RouteCreate routeId
        addStopQueries = map (Lib2.RouteAddStop routeId) routeStops  -- Adding stops to the route
        addNestedRouteQueries = concatMap routeToQueriesNested nestedRoutes  -- renerate queries for nested routes
        addRouteAddRouteQueries = concatMap (routeAddRouteQueries routeId) nestedRoutes  -- renerate queries for adding nested routes
    in createRouteQuery : (addStopQueries ++ addNestedRouteQueries ++ addRouteAddRouteQueries)

-- Helper function to renerate queries for nested routes
routeToQueriesNested :: Lib2.Route -> [Lib2.Query]
routeToQueriesNested (Lib2.Route nestedRouteId nestedRouteStops nestedNestedRoutes) =
    let createNestedRouteQuery = Lib2.RouteCreate nestedRouteId
        addStopQueries = map (Lib2.RouteAddStop nestedRouteId) nestedRouteStops  -- Adding stops to the nested route
        addNestedRouteQueries = concatMap routeToQueriesNested nestedNestedRoutes  -- Recurse for any further nested routes
    in createNestedRouteQuery : (addStopQueries ++ addNestedRouteQueries)

-- renerates queries for adding a child route to a parent route
routeAddRouteQueries :: Lib2.Name -> Lib2.Route -> [Lib2.Query]
routeAddRouteQueries parentId childRoute =
    let addRouteQuery = [Lib2.RouteAddRoute (Lib2.Route parentId [] []) childRoute]
    in addRouteQuery

-- Converts a RouteTreeList to a series of queries
listToQueries :: (Lib2.Name, [Lib2.RouteTree]) -> [Lib2.Query]
listToQueries (listName, trees) =
    let createListQuery = Lib2.ListCreate listName
        treeQueries = concatMap (treeToQueriesWithListAdd listName) trees
    in createListQuery : treeQueries

-- Converts a RouteTree to queries, ensuring it includes a ListAdd query for the list
treeToQueriesWithListAdd :: Lib2.Name -> Lib2.RouteTree -> [Lib2.Query]
treeToQueriesWithListAdd listName tree =
    let routeCreateQueries = treeToCreateQueries tree
        addRouteQueries = routeAddRouteQueriesFromTree tree
        listAddQuery = Lib2.ListAdd listName (routeTreeToRoute tree)
    in routeCreateQueries ++ addRouteQueries ++ [listAddQuery]

-- Converts a RouteTree to the queries to create the route, ensuring correct order
treeToCreateQueries :: Lib2.RouteTree -> [Lib2.Query]
treeToCreateQueries Lib2.EmptyTree = []
treeToCreateQueries (Lib2.Node (Lib2.NodeRoute routeId routeStops) childRoutes) =
    let createRouteQuery = Lib2.RouteCreate routeId
        addStopQueries = map (Lib2.RouteAddStop routeId) routeStops
        childRouteQueries = concatMap treeToCreateQueries childRoutes
    in createRouteQuery : (addStopQueries ++ childRouteQueries)

-- Converts a RouteTree to a Route representation
routeTreeToRoute :: Lib2.RouteTree -> Lib2.Route
routeTreeToRoute (Lib2.Node (Lib2.NodeRoute routeId routeStops) childRoutes) =
    Lib2.Route routeId routeStops (map routeTreeToRoute childRoutes)
routeTreeToRoute Lib2.EmptyTree = error "Cannot convert an empty tree to a route"

-- Ensure that route-add-route queries are renerated between route creations
routeAddRouteQueriesFromTree :: Lib2.RouteTree -> [Lib2.Query]
routeAddRouteQueriesFromTree (Lib2.Node (Lib2.NodeRoute routeId _) childRoutes) =
    concatMap (\(Lib2.Node (Lib2.NodeRoute childRouteId _) _) -> 
        [Lib2.RouteAddRoute (Lib2.Route routeId [] []) (Lib2.Route childRouteId [] [])]) childRoutes
routeAddRouteQueriesFromTree _ = []


-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) =
    "BEGIN \n" ++ renQuery query ++ ";\nEND"
renderStatements (Batch queries) =
    "BEGIN \n" ++ unlines (map (\q -> renQuery q ++ ";") queries) ++ "END"


renQuery :: Lib2.Query -> String
renQuery (Lib2.ListCreate name) =
    "list-create " ++ renName name
renQuery (Lib2.ListAdd name route) =
    "list-add " ++ renName name ++ " " ++ renRoute route
renQuery (Lib2.ListGet name) =
    "list-get " ++ renName name
renQuery (Lib2.ListRemove name) =
    "list-remove " ++ renName name
renQuery (Lib2.RouteCreate name) =
    "route-create " ++ renName name
renQuery (Lib2.RouteGet name) =
    "route-get " ++ renName name
renQuery (Lib2.RouteAddRoute parentRoute childRoute) =
    "route-add-route " ++ renRoute parentRoute ++ " " ++ renRoute childRoute
renQuery (Lib2.RouteAddStop name stop) =
    "route-add-stop " ++ renName name ++ " " ++ renStop stop
renQuery (Lib2.RouteRemoveStop name stop) =
    "route-remove-stop " ++ renName name ++ " " ++ renStop stop
renQuery (Lib2.RouteRemove name) =
    "route-remove " ++ renName name
renQuery (Lib2.StopCreate name) =
    "stop-create " ++ renName name
renQuery (Lib2.StopDelete name) =
    "stop-delete " ++ renName name
renQuery (Lib2.RoutesFromStop stop) =
    "routes-from-stop " ++ renStop stop

renRoute :: Lib2.Route -> String
renRoute (Lib2.Route routeId stops nestedRoutes) =
        "<" ++ renName routeId ++ "{" ++ renStops stops ++ renNestedRoutes nestedRoutes ++ "}>"

renStops :: [Lib2.Stop] -> String
renStops stops = intercalate " " (map renStop stops)

renStop :: Lib2.Stop -> String
renStop (Lib2.Stop stopId) = "(" ++ renName stopId ++ ")"

renNestedRoutes :: [Lib2.Route] -> String
renNestedRoutes nestedRoutes = intercalate " " (map renRoute nestedRoutes)

renName :: Lib2.Name -> String
-- renName (Lib2.NumberName n) = show n
-- renName (Lib2.WordName w) = w
renName (Lib2.StringName s) = s

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
            IO (Either String (Maybe String, String))
stateTransition stateVar command ioChan = case command of
  LoadCommand -> do
    loadChan <- newChan
    writeChan ioChan (Load loadChan)
    info <- readChan loadChan
    case parseStatements info of
      Right (statements, _) -> atomically $ do
        initialState <- readTVar stateVar
        case statements of
          Batch queries -> do
            let applyQueries = foldl (\stateRes query -> case stateRes of
                  Right state -> case Lib2.stateTransition state query of
                    Right newState -> Right newState
                    Left err -> Left err
                  Left err -> Left err) (Right initialState) queries
            case applyQueries of
              Right newState -> do
                writeTVar stateVar newState
                return $ Right (Just "Loading was successful.", show newState)
              Left err -> return $ Left ("Error applying queries: " ++ err)
          Single query -> do
            case Lib2.stateTransition initialState query of
              Right newState -> do
                writeTVar stateVar newState
                return $ Right (Just "Loading was successful.", show query)
              Left err -> return $ Left ("Error applying query: " ++ err)
      Left err -> return $ Left ("Error parsing statements: " ++ err)

  SaveCommand -> do
    currentState <- readTVarIO stateVar
    let state = renderStatements (marshallState currentState)
    saveChan <- newChan
    writeChan ioChan (Save state saveChan)
    _ <- readChan saveChan
    return $ Right (Just "State saving was successful.", show currentState)

  StatementCommand statements -> atomically $ do
      currentState <- readTVar stateVar
      case statements of
        Batch queries -> do
          -- Apply the queries
          let applyQueries = foldl (\stateRes query -> case stateRes of
                Right state -> case Lib2.stateTransition state query of
                  Right newState -> Right newState
                  Left err -> Left err
                Left err -> Left err) (Right currentState) queries

          case applyQueries of
            Right newState -> do
              writeTVar stateVar newState
              return $ Right (Just "Statements executed successfully.", show newState)
            Left err -> return $ Left ("Error executing statements: " ++ err)
        Single query -> do
          case Lib2.stateTransition currentState query of
            Right newState -> do
              writeTVar stateVar newState
              return $ Right (Just "Statement executed successfully.", show newState)
            Left err -> return $ Left ("Error executing statement: " ++ err)