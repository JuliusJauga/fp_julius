{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
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
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar, readTVarIO )
import Data.List (isPrefixOf, isSuffixOf, notElem, (\\), intercalate, find, delete, partition, nubBy)
import qualified Lib2
import Data.Char (isSpace)
import Control.Exception (try, SomeException)
import Debug.Trace
import Data.Either (partitionEithers)
import Lib2 (Stop(stopId'))
import GHC.Read (list)

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
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "load" `isPrefixOf` input = Right (LoadCommand, drop 4 input)
  | "save" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
  | "BEGIN" `isPrefixOf` input = case parseStatements input of
      Left err -> Left err
      Right (stmts, rest) -> Right (StatementCommand stmts, rest)
  | otherwise = case Lib2.parseQuery (trim input) of
      Left err -> Left err
      Right query -> Right (StatementCommand (Single query), "")

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
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
                            if ';' `elem` body
                                then
                                    let queries = map (Lib2.parseQuery . trim) (filter (not . null) $ splitOn ';' body)
                                        (errors, parsedQueries) = partitionEithers queries
                                    in if null errors
                                        then
                                            let queriesList = parsedQueries
                                            in Right (Batch queriesList, "")
                                        else Left $ "Error parsing queries: " ++ unlines errors
                                else
                                    case Lib2.parseQuery body of
                                        Left err -> Left err
                                        Right query -> Right (Single query, "")
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
        routeQueriesWithStopCreate = concatMap routeToQueries routes
        listQueriesWithStopCreate = concatMap listToQueries routeTreeLists
        stopQueries' = cancelDuplicateStopCreateQueries (stopQueries ++ listQueriesWithStopCreate ++ routeQueriesWithStopCreate)
        listQueries = filter (\q -> case q of
            Lib2.StopCreate _ -> False
            _ -> True) listQueriesWithStopCreate
        routeQueries = filter (\q -> case q of
            Lib2.StopCreate _ -> False
            _ -> True) routeQueriesWithStopCreate
    in Batch (stopQueries' ++ routeQueries ++ listQueries)


-- Converts a Stop into a StopCreate query
stopToQuery :: Lib2.Stop -> Lib2.Query
stopToQuery (Lib2.Stop stopId) = Lib2.StopCreate stopId


cancelDuplicateStopCreateQueries :: [Lib2.Query] -> [Lib2.Query]
cancelDuplicateStopCreateQueries queries =
    let
        -- A helper function to filter unique `StopCreate` queries
        removeDuplicateStopCreates :: [Lib2.Query] -> [Lib2.Query] -> [Lib2.Query]
        removeDuplicateStopCreates [] _ = []
        removeDuplicateStopCreates (q:qs) seen = case q of
            Lib2.StopCreate stopId ->
                if Lib2.StopCreate stopId `elem` seen
                    then removeDuplicateStopCreates qs seen
                    else q : removeDuplicateStopCreates qs (Lib2.StopCreate stopId : seen)
            _ -> removeDuplicateStopCreates qs seen

        -- Extract `StopCreate` queries and deduplicate them
        stopCreateQueries = filter (\q -> case q of
            Lib2.StopCreate _ -> True
            _ -> False) queries
        uniqueStopCreates = removeDuplicateStopCreates stopCreateQueries []
    in uniqueStopCreates


-- Converts a Route into a series of queries, including RouteAddStop queries
routeToQueries :: Lib2.Route -> [Lib2.Query]
routeToQueries (Lib2.Route routeId routeStops nestedRoutes) =
    let createRouteQuery = Lib2.RouteCreate (Lib2.Route routeId routeStops nestedRoutes)
        createStopQueries = map (Lib2.StopCreate . stopId') routeStops  -- Adding stops to the route
    in createStopQueries ++ [createRouteQuery]

-- renerates queries for adding a child route to a parent route
routeAddRouteQueries :: Lib2.Name -> Lib2.Route -> [Lib2.Query]
routeAddRouteQueries parentId childRoute =
    let addRouteQuery = [Lib2.RouteAddRoute parentId (Lib2.routeId' childRoute)]
    in addRouteQuery

listToQueries :: (Lib2.Name, [Lib2.RouteTree]) -> [Lib2.Query]
listToQueries (listName, trees) =
    let createListQuery = Lib2.ListCreate listName
        treeQueries = concatMap (treeToQueriesWithListAdd listName) trees
    in createListQuery : treeQueries

treeToQueriesWithListAdd :: Lib2.Name -> Lib2.RouteTree -> [Lib2.Query]
treeToQueriesWithListAdd listName tree =
    let routeCreateQueries = treeToCreateQueries tree
        listAddQuery = Lib2.ListAdd listName (Lib2.routeId' (Lib2.routeTreeToRoute tree))
    in routeCreateQueries ++ [listAddQuery]

treeToCreateQueries :: Lib2.RouteTree -> [Lib2.Query]
treeToCreateQueries Lib2.EmptyTree = []
treeToCreateQueries (Lib2.Node (Lib2.NodeRoute routeId routeStops) childRoutes) =
    let createStopsQueries = map stopToQuery routeStops
        node = (Lib2.Node (Lib2.NodeRoute routeId routeStops) childRoutes)
        route = Lib2.routeTreeToRoute node
        createRouteQueries = routeToQueries route
    in createStopsQueries ++ createRouteQueries

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) =
    "BEGIN \n" ++ renQuery query ++ "\nEND"
renderStatements (Batch queries) =
    "BEGIN \n" ++ unlines (map (\q -> renQuery q ++ ";") queries) ++ "END"


renQuery :: Lib2.Query -> String
renQuery (Lib2.ListCreate name) =
    "list-create " ++ renName name
renQuery (Lib2.ListAdd name name') =
    "list-add " ++ renName name ++ " " ++ renName name'
renQuery (Lib2.ListGet name) =
    "list-get " ++ renName name
renQuery (Lib2.ListRemove name) =
    "list-remove " ++ renName name
renQuery (Lib2.RouteCreate route) =
    "route-create " ++ renRoute route
renQuery (Lib2.RouteGet name) =
    "route-get " ++ renName name
renQuery (Lib2.RouteAddRoute parentRoute childRoute) =
    "route-add-route " ++ renName parentRoute ++ " " ++ renName childRoute
renQuery (Lib2.RouteAddStop name stop) =
    "route-add-stop " ++ renName name ++ " " ++ renName stop
renQuery (Lib2.RouteRemoveStop name stop) =
    "route-remove-stop " ++ renName name ++ " " ++ renName stop
renQuery (Lib2.RouteRemove name) =
    "route-remove " ++ renName name
renQuery (Lib2.StopCreate name) =
    "stop-create " ++ renName name
renQuery (Lib2.StopDelete name) =
    "stop-delete " ++ renName name
renQuery (Lib2.RoutesFromStop stop) =
    "routes-from-stop " ++ renName stop

renRoute :: Lib2.Route -> String
renRoute (Lib2.Route routeId stops nestedRoutes) =
        "<" ++ renName routeId ++ "{" ++ renStops stops ++ renNestedRoutes nestedRoutes ++ "}>"

renStops :: [Lib2.Stop] -> String
renStops stops = intercalate "" (map renStop stops)

renStop :: Lib2.Stop -> String
renStop (Lib2.Stop stopId) = "(" ++ renName stopId ++ ")"

renNestedRoutes :: [Lib2.Route] -> String
renNestedRoutes nestedRoutes = intercalate "" (map renRoute nestedRoutes)

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
            Left err -> return $ Left err