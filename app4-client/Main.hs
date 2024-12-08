{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.String.Conversions (cs)
import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import qualified DSL as DSL
import qualified Network.Wreq as NetworkWreq
import qualified IMI as IMI
import qualified GHC.Generics as DSL


interpretOneByOne :: DSL.Domain a -> IO a
interpretOneByOne (Pure a) = return a
interpretOneByOne (Free (DSL.ListCreate name next)) = do
    putStrLn $ "Creating list " ++ name
    response <- NetworkWreq.post "http://localhost:3000" (pack $ "list-create " ++ name)
    print response
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

interpretOneByOne (Free (DSL.Save next)) = do
    putStrLn "Saving"
    _ <- NetworkWreq.post "http://localhost:3000" (pack "save")
    interpretOneByOne next

interpretOneByOne (Free (DSL.Load next)) = do
    putStrLn "Loading"
    _ <- NetworkWreq.post "http://localhost:3000" (pack "load")
    interpretOneByOne next

interpretBatch :: DSL.Domain a -> IO String
interpretBatch program = do
    let commands = collectCommands program
    if null commands
        then return "No commands to execute"
        else do
            response <- NetworkWreq.post "http://localhost:3000" (pack $ unlines ("BEGIN" : commands ++ ["END"]))
            print response
            return "Commands executed"
            

collectCommands :: DSL.Domain a -> [String]
collectCommands = reverse . snd . foldCommands

foldCommands :: DSL.Domain a -> ([String], [String])
foldCommands (Pure _) = ([], [])
foldCommands (Free (DSL.ListCreate name next)) =
    let (stack, cmds) = foldCommands next
    in (("list-create " ++ name) : stack, ("list-create " ++ name ++ ";") : cmds)
foldCommands (Free (DSL.ListAdd ingName listName next)) =
    let (stack, cmds) = foldCommands next
    in (stack, ("list-add " ++ ingName ++ " " ++ listName ++ ";") : cmds)
foldCommands (Free (DSL.ListRemove listName next)) =
    let (stack, cmds) = foldCommands next
    in (stack, ("list-remove " ++ listName ++ ";") : cmds)
foldCommands (Free (DSL.RouteCreate name next)) =
    let (stack, cmds) = foldCommands next
    in if "route-remove " ++ name `elem` stack
       then (filter (/= "route-remove " ++ name) stack, cmds) -- Cancel out with remove
       else (("route-create " ++ name) : stack, ("route-create " ++ name ++ ";") : cmds)
foldCommands (Free (DSL.RouteRemove name next)) =
    let (stack, cmds) = foldCommands next
    in if "route-create " ++ name `elem` stack
       then (filter (/= "route-create " ++ name) stack, cmds) -- Cancel out with create
       else (stack, cmds) -- Skip unmatched delete
foldCommands (Free (DSL.StopCreate name next)) =
    let (stack, cmds) = foldCommands next
    in if "stop-delete " ++ name `elem` stack
       then (filter (/= "stop-delete " ++ name) stack, filter (/= "stop-delete " ++ name ++ ";") cmds) -- Cancel out with delete
       else (("stop-create " ++ name) : stack, ("stop-create " ++ name ++ ";") : cmds)
foldCommands (Free (DSL.StopDelete name next)) =
    let (stack, cmds) = foldCommands next
    in if "stop-create " ++ name `elem` stack
       then (filter (/= "stop-create " ++ name) stack, filter (/= "stop-create " ++ name ++ ";") cmds) -- Cancel out with create
       else (("stop-delete " ++ name) : stack, ("stop-delete " ++ name ++ ";") : cmds)
foldCommands (Free (DSL.Save next)) =
    let (stack, cmds) = foldCommands next
    in (stack, "save;" : cmds)
foldCommands (Free (DSL.Load next)) =
    let (stack, cmds) = foldCommands next
    in (stack, "load;" : cmds)
foldCommands (Free (DSL.RouteAddRoute parentRouteName childRouteName next)) =
    let (stack, cmds) = foldCommands next
    in (stack, ("route-add-route " ++ parentRouteName ++ " " ++ childRouteName ++ ";") : cmds)
foldCommands (Free (DSL.RouteAddStop routeName stopName next)) =
    let (stack, cmds) = foldCommands next
    in (stack, ("route-add-stop " ++ routeName ++ " " ++ stopName ++ ";") : cmds)
foldCommands (Free (DSL.RouteRemoveStop routeName stopName next)) =
    let (stack, cmds) = foldCommands next
    in (stack, ("route-remove-stop " ++ routeName ++ " " ++ stopName ++ ";") : cmds)
foldCommands (Free (DSL.RoutesFromStop stopName f)) =
    let (stack, cmds) = foldCommands (f "routes")
    in (stack, ("routes-from-stop " ++ stopName ++ ";") : cmds)


interpretCommands :: DSL.Domain a -> IO String
interpretCommands domain = do
    let commands = collectCommands domain
    if length commands == 1
        then do
            interpretOneByOne domain
            return "Command executed"
        else do
            result <- interpretBatch domain
            if result == "No commands to execute"
                then return "No commands to execute"
                else return "Commands executed"


main :: IO ()
main = do
    let program = do
            DSL.listCreate "list1"
            DSL.routeCreate "<route1{}>"
    result <- interpretCommands program
    print result
    let program = do
            DSL.stopCreate "stop1"
            DSL.routeAddStop "route1" "stop1"
    result <- interpretCommands program
    print result
    let cancellingProgram = do
            DSL.stopCreate "stop2"
            DSL.stopDelete "stop2"
    result <- interpretCommands cancellingProgram
    print result
    let program = do
            DSL.save
    result <- interpretCommands program
    print result