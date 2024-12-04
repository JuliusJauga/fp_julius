module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.String.Conversions (cs)
import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import qualified DSL
import qualified Network.Wreq as NetworkWreq


interpretOneByOne DSL.Program a -> IO a
interpretOneByOne (Pure a) = return a
interpretOneByOne (Free (DSL.ListCreate name next)) = do
    putStrLn $ "Creating list " ++ name
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "list-create " ++ name)
    interpretOneByOne next
interpretOneByOne (Free (DSL.ListAdd ingName listName next)) = do
    putStrLn $ "Adding " ++ ingName ++ " to " ++ listName
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "list-add " ++ ingName ++ " " ++ listName)
    interpretOneByOne next
interpretOneByOne (Free (DSL.ListRemove listName next)) = do
    putStrLn $ "Removing list " ++ listName
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "list-remove " ++ listName)
    interpretOneByOne next
interpretOneByOne (Free (DSL.ListGet name f)) = do
    putStrLn $ "Getting list " ++ name
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "list-get " ++ name)
    interpretOneByOne (f "list")
interpretOneByOne (Free (DSL.RouteCreate name next)) = do
    putStrLn $ "Creating route " ++ name
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "route-create " ++ name)
    interpretOneByOne next
interpretOneByOne (Free (DSL.RouteGet name f)) = do
    putStrLn $ "Getting route " ++ name
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "route-get " ++ name)
    interpretOneByOne (f "route")
interpretOneByOne (Free (DSL.RouteAddRoute parentRouteName childRouteName next)) = do
    putStrLn $ "Adding route " ++ childRouteName ++ " " ++ parentRouteName
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "route-add-route " ++ parentRouteName ++ " " ++ childRouteName)
    interpretOneByOne next
interpretOneByOne (Free (DSL.RouteAddStop routeName stopName next)) = do
    putStrLn $ "Adding stop " ++ stopName ++ " to route " ++ routeName
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "route-add-stop " ++ routeName ++ " " ++ stopName)
    interpretOneByOne next
interpretOneByOne (Free (DSL.RouteRemoveStop routeName stopName next)) = do
    putStrLn $ "Removing stop " ++ stopName ++ " from route " ++ routeName
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "route-remove-stop " ++ routeName ++ " " ++ stopName)
    interpretOneByOne next
interpretOneByOne (Free (DSL.StopCreate name next)) = do
    putStrLn $ "Creating stop " ++ name
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "stop-create " ++ name)
    interpretOneByOne next
interpretOneByOne (Free (DSL.StopDelete name next)) = do
    putStrLn $ "Deleting stop " ++ name
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "stop-delete " ++ name)
    interpretOneByOne next
interpretOneByOne (Free (DSL.RoutesFromStop stopName f)) = do
    putStrLn $ "Getting routes from stop " ++ stopName
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ "routes-from-stop " ++ stopName)
    interpretOneByOne (f "routes")

interpretBatch :: DSL.Program a -> IO a
interpretBatch program = do
    let commands = collectCommands program
    _ <- NetworkWreq.post "http://localhost:3000" (pack $ unlines commands)
    return undefined

collectCommands :: DSL.Program a -> [String]
collectCommands (Pure _) = []
collectCommands (Free (DSL.ListCreate name next)) = ("list-create " ++ name) : collectCommands next
collectCommands (Free (DSL.ListAdd ingName listName next)) = ("list-add " ++ ingName ++ " " ++ listName) : collectCommands next
collectCommands (Free (DSL.ListRemove listName next)) = ("list-remove " ++ listName) : collectCommands next
collectCommands (Free (DSL.ListGet name f)) = ("list-get " ++ name) : collectCommands (f "list")
collectCommands (Free (DSL.RouteCreate name next)) = ("route-create " ++ name) : collectCommands next
collectCommands (Free (DSL.RouteGet name f)) = ("route-get " ++ name) : collectCommands (f "route")
collectCommands (Free (DSL.RouteAddRoute parentRouteName childRouteName next)) = ("route-add-route " ++ parentRouteName ++ " " ++ childRouteName) : collectCommands next
collectCommands (Free (DSL.RouteAddStop routeName stopName next)) = ("route-add-stop " ++ routeName ++ " " ++ stopName) : collectCommands next
collectCommands (Free (DSL.RouteRemoveStop routeName stopName next)) = ("route-remove-stop " ++ routeName ++ " " ++ stopName) : collectCommands next
collectCommands (Free (DSL.StopCreate name next)) = ("stop-create " ++ name) : collectCommands next
collectCommands (Free (DSL.StopDelete name next)) = ("stop-delete " ++ name) : collectCommands next
collectCommands (Free (DSL.RoutesFromStop stopName f)) = ("routes-from-stop " ++ stopName) : collectCommands (f "routes")


main :: IO ()
main = do
    let program = do
        DSL.listCreate "list1"
        DSL.routeCreate "<route1{}>"
        DSL.listAdd "list1" "route1"
    result <- interpretOneByOne program
    print result