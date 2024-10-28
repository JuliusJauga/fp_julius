# fp_julius
1 laboratory work.

## Domain 

City Public Transport Schedule System

This system helps manage and organize the public transport routes and stops in a city. It allows you to add, remove, list routes and stops. Additionally, it provides ability to retrieve information about routes and stops.

## Lab2 comments

I made a parser that parses elements from the BNF. It also parses different commands, and state transition is implemented for this using a tree structure ADT. The parser handles the recursive nature of the routes and stops, ensuring that each element is correctly identified and processed.
When adding a route to a list of routes, the route gets processed into a tree structure. Making the nested routes add to a tree recursively

#### ADT implementation
Route1 is defined like this: Route {routeId' = "R1", stops' = \[stop1, stop11\], nestedRoutes' = \[route2\]}

Route2 is defined like this: Route {routeId' = "R2", stops' = \[stop2\], nestedRoutes' = \[route3\]}

Route2 is defined like this: Route {routeId' = "R3", stops' = \[stop3, stop33\], nestedRoutes' = \[\]}

Route4 is defined like this: Route {routeId' = "R4", stops' = \[stop4\], nestedRoutes' = \[route5, route6\]}

Route5 is defined like this: Route {routeId' = "R5", stops' = \[\], nestedRoutes' = \[\]}

Route6 is defined like this: Route {routeId' = "R6", stops' = \[\], nestedRoutes' = \[\]}

When inserting Route1 and Route4 to the tree the structure becomes this:
```    
[   _________List1________      ,       ______List2______   ]
      |               |                     /
    Route1          Route4          Some other route
    /               /   \               /   |   \
Route2          Route5   Route6       ...  ...  ...
    \
    Route3
```
Every node in the tree structure is of type RouteTree, a RouteTree has three constructors: A NodeRoute (Which is the root node) and a list of children nodes, which are of type RouteTree.
A NodeRoute type has two constructors, a nodeRouteId (Which is the name of the route) and a list of nodeStops, which are the original stops from route from which it was made of.

When rebuilding a Route back from a tree, the topmost RouteTree and all of its children nodes will get recursively added to nestedRoutes of Route type.

#### Unit tests.

Added unit testing for each element of parsing. Components and commands.
Also added unit testing for state transitions.
Higher recursion unit tests are provided in the bottom of fp_julius/test/Spec.hs file. These tests include inserting higher nested Routes, each having their own stops and so on.

#### Details
Added parsing and state transition control for the BNF that is provided in fp_julius/grammar.txt.
Changes that were made to the BNF are documented on fp_julius/ChangesMadeToGrammar.txt

#### State transition usage in GHCI

Commands and expected output are provided in fp_julius/stateTransitionWalkthrough.hs


## BNF
The grammar for this domain is provided in fp_julius/grammar.txt.

It is written in BNF.

BNF tested on https://bnfplayground.pauliankline.com/

### Recursion

Recursion is in routes being made of other routes, each having their stops with departure times.

See fp_julius/grammar.txt

### BNF Example

```
[
<Route1{(stop1)(stop2)(stop3)
    <InternalRoute1{(stop1)}>}>
<Route2{(stop4)(stop5)(stop6)
    <InternalRoute2{(stop4)(stop5)
        <InternalRoute22{(stop4)}>}>}>
]
```

In this example the system/route list is made out of 2 main routes, each having internal routes, also second internal route also has a internal route. 

### Basic structure

```
routes 
    route_name
    routes
        route_name
        routes
            route_name
            routes
                ...
            stops
                ...
            ...
        stops
            ...
        ...
    stops 
        stop_name
    ...
```
A route can be made of other smaller routes, each having their own stops.
