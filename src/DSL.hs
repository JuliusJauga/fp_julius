{-# LANGUAGE DeriveFunctor #-}
module DSL where

import Control.Monad.Free (Free(..), liftF)

-- -- Define the DSL commands
-- data Command next = Create String Int String next
--                   | Add String String next
--                   | Remove String String next
--                   | Get String (String -> next)
--                   | CreateEmptyList String next
--                   | Delete String next
--                   deriving Functor

data Command next = ListCreate String next
                    | ListAdd String String next
                    | ListRemove String String next
                    | ListGet String (String -> next)
                    | RouteCreate String next
                    | RouteGet String (String -> next)
                    | RouteAddRoute String String next
                    | RouteAddStop String String next
                    | RouteRemoveStop String String next
                    | StopCreate String next
                    | StopDelete String next
                    | RoutesFromStop String (String -> next)

-- Define the Program type
type Program = Free Command


listCreate :: String -> Program ()
listCreate name = liftF $ ListCreate name ()

listAdd :: String -> String -> Program ()
listAdd ingName listName = liftF $ ListAdd ingName listName ()

listRemove :: String -> String -> Program ()
listRemove listName = liftF $ ListRemove listName ()

listGet :: String -> Program String
listGet name = liftF $ ListGet name id

routeCreate :: String -> Program ()
routeCreate name = liftF $ RouteCreate name ()

routeGet :: String -> Program String
routeGet name = liftF $ RouteGet name id

routeAddRoute :: String -> String -> Program ()
routeAddRoute routeName stopName = liftF $ RouteAddRoute routeName stopName ()

routeAddStop :: String -> String -> Program ()
routeAddStop routeName stopName = liftF $ RouteAddStop routeName stopName ()

routeRemoveStop :: String -> String -> Program ()
routeRemoveStop routeName stopName = liftF $ RouteRemoveStop routeName stopName ()

stopCreate :: String -> Program ()
stopCreate name = liftF $ StopCreate name ()

stopDelete :: String -> Program ()
stopDelete name = liftF $ StopDelete name ()

routesFromStop :: String -> Program String
routesFromStop name = liftF $ RoutesFromStop name id
