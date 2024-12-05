{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad.IO.Class ()
import Control.Monad.State.Strict
import Data.List qualified as L
import Lib1 qualified
import Lib2 qualified
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )

type Repl a = HaskelineT (StateT Lib2.State IO) a

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer n =
  return $ Prelude.filter (L.isPrefixOf n) Lib1.completions

cmd :: String -> Repl ()
cmd str = do
  case Lib2.parseQuery str of
    Left e -> liftIO $ putStrLn $ "PARSE ERROR:" ++ e
    Right query -> do
      st <- lift get
      case Lib2.stateTransition st query of
        Left e2 -> liftIO $ putStrLn $ e2
        Right ns -> do
          lift (put ns)
          currentState <- lift get
          liftIO $ do
            putStrLn "Transition succeeded."
            printState currentState

main :: IO ()
main = evalStateT
  (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) Lib2.initialState


printState :: Lib2.State -> IO ()
printState (Lib2.State routeTreeLists' routes' stops'') = do
    putStrLn "Route Tree Lists:"
    mapM_ (printRouteTreeList 0) routeTreeLists'
    putStrLn "Routes:"
    mapM_ (printRoute 0) routes'
    putStrLn "Stops:"
    mapM_ (printStop) stops''
    putStrLn ""

printRouteTreeList :: Int -> (Lib2.Name, [Lib2.RouteTree]) -> IO ()
printRouteTreeList indentLevel (name, trees) = do
    putStrLn $ replicate indentLevel ' ' ++ "List Name: " ++ show name
    mapM_ (printRouteTree (indentLevel + 2)) trees

printRouteTree :: Int -> Lib2.RouteTree -> IO ()
printRouteTree indentLevel Lib2.EmptyTree = putStrLn $ replicate indentLevel ' ' ++ "Empty Tree"
printRouteTree indentLevel (Lib2.Node (Lib2.NodeRoute routeId stops'') children) = do
    putStrLn $ replicate indentLevel ' ' ++ "Route ID: " ++ show routeId
    putStrLn $ replicate indentLevel ' ' ++ "Stops: " ++ show stops''
    putStrLn $ replicate indentLevel ' ' ++ "Children:"
    mapM_ (printRouteTree (indentLevel + 2)) children

printRoute :: Int -> Lib2.Route -> IO ()
printRoute indentLevel (Lib2.Route routeId stops'' nestedRoutes) = do
    putStrLn $ replicate indentLevel ' ' ++ "Route ID: " ++ show routeId
    putStrLn $ replicate indentLevel ' ' ++ "Stops: " ++ show stops''
    putStrLn $ replicate indentLevel ' ' ++ "Nested Routes:"
    mapM_ (printRoute (indentLevel + 2)) nestedRoutes

printStop :: Lib2.Stop -> IO ()
printStop (Lib2.Stop stopId) = putStrLn $ "  Stop ID: " ++ show stopId