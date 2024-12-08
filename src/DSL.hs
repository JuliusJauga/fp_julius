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

data DomainAlgebra next = ListCreate String next
                    | ListAdd String String next
                    | ListRemove String next
                    | ListGet String (String -> next)
                    | RouteCreate String next
                    | RouteRemove String next
                    | RouteGet String (String -> next)
                    | RouteAddRoute String String next
                    | RouteAddStop String String next
                    | RouteRemoveStop String String next
                    | StopCreate String next
                    | StopDelete String next
                    | RoutesFromStop String (String -> next)
                    | Save next
                    | Load next
                    deriving Functor

-- Define the Domain type
type Domain = Free DomainAlgebra


listCreate :: String -> Domain ()
listCreate name = liftF $ ListCreate name ()

listAdd :: String -> String -> Domain ()
listAdd ingName listName = liftF $ ListAdd ingName listName ()

listRemove :: String -> Domain ()
listRemove name = liftF $ ListRemove name ()

listGet :: String -> Domain String
listGet name = liftF $ ListGet name id

routeCreate :: String -> Domain ()
routeCreate name = liftF $ RouteCreate name ()

routeRemove :: String -> Domain ()
routeRemove name = liftF $ RouteRemove name ()

routeGet :: String -> Domain String
routeGet name = liftF $ RouteGet name id

routeAddRoute :: String -> String -> Domain ()
routeAddRoute routeName stopName = liftF $ RouteAddRoute routeName stopName ()

routeAddStop :: String -> String -> Domain ()
routeAddStop routeName stopName = liftF $ RouteAddStop routeName stopName ()

routeRemoveStop :: String -> String -> Domain ()
routeRemoveStop routeName stopName = liftF $ RouteRemoveStop routeName stopName ()

stopCreate :: String -> Domain ()
stopCreate name = liftF $ StopCreate name ()

stopDelete :: String -> Domain ()
stopDelete name = liftF $ StopDelete name ()

routesFromStop :: String -> Domain String
routesFromStop name = liftF $ RoutesFromStop name id

save :: Domain ()
save = liftF $ Save ()

load :: Domain ()
load = liftF $ Load ()
