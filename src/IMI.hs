{-# LANGUAGE DeriveFunctor #-}
module IMI where

import qualified DSL as D
import Control.Monad.Free (Free(..))  
import Control.Monad.Trans.State.Strict (StateT, get, modify)
import Control.Monad.Except (ExceptT, liftIO, throwError, runExceptT)

type InMemoryState = StateT [(String, String)] (ExceptT String IO)

interpretInMemory :: D.Domain a -> InMemoryState a
interpretInMemory (Pure a) = return a
interpretInMemory (Free (D.ListCreate name next)) = do
    modify ((name, "") :)
    interpretInMemory next
interpretInMemory (Free (D.ListAdd ingName listName next)) = do
    modify (addToList ingName listName)
    interpretInMemory next
interpretInMemory (Free (D.ListRemove listName next)) = do
    modify (removeFromList listName)
    interpretInMemory next
interpretInMemory (Free (D.ListGet name next)) = do
    state <- get
    let items = lookup name state
    case items of
        Just i  -> interpretInMemory (next i)
        Nothing -> throwError $ "List " ++ name ++ " not found"
interpretInMemory (Free (D.RouteCreate name next)) = do
    modify ((name, "") :)
    interpretInMemory next
interpretInMemory (Free (D.RouteGet name next)) = do
    state <- get
    let route = lookup name state
    case route of
        Just r  -> interpretInMemory (next r)
        Nothing -> throwError $ "Route " ++ name ++ " not found"
interpretInMemory (Free (D.RouteAddRoute parentRouteName childRouteName next)) = do
    modify (addToList childRouteName parentRouteName)
    interpretInMemory next
interpretInMemory (Free (D.RouteAddStop routeName stopName next)) = do
    modify (addToList stopName routeName)
    interpretInMemory next
interpretInMemory (Free (D.RouteRemoveStop routeName stopName next)) = do
    modify (removeItemFromList stopName routeName)
    interpretInMemory next
interpretInMemory (Free (D.StopCreate name next)) = do
    modify ((name, "") :)
    interpretInMemory next
interpretInMemory (Free (D.StopDelete name next)) = do
    modify (filter ((/= name) . fst))
    interpretInMemory next
interpretInMemory (Free (D.RoutesFromStop name next)) = do
    state <- get
    let routes = [route | (route, stops) <- state, name `elem` words stops]
    interpretInMemory (next (unwords routes))

addToList :: String -> String -> [(String, String)] -> [(String, String)]
addToList item listName state = map (\(name, items) -> if name == listName then (name, items ++ " " ++ item) else (name, items)) state

removeFromList :: String -> [(String, String)] -> [(String, String)]
removeFromList listName state = filter ((/= listName) . fst) state

removeItemFromList :: String -> String -> [(String, String)] -> [(String, String)]
removeItemFromList item listName state = map (\(name, items) -> if name == listName then (name, unwords $ filter (/= item) (words items)) else (name, items)) state