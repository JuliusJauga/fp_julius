# fp_julius
1 laboratory work.

## Domain 

City Public Transport Schedule System

This system helps manage and organize the public transport routes and stops in a city. It allows you to add, remove, list routes and stops. Additionally, it provides ability to retrieve information about routes and stops.

## BNF
The grammar for this domain is provided in fp_julius/grammar.txt.

It is written in BNF.

BNF tested on https://bnfplayground.pauliankline.com/


## Commands

- **`list-create`** - Creates an empty route list.
- **`list-add <list> <route>`** - Add a route to a list.
- **`list-get <list> <name>`** - Get a route from a list.
- **`list-remove <list> <name>`** - Remove a route from a list.

- **`route-create <name>`** - Creates a route with the provided name.
- **`route-get <route> <name>`** - Get a stop from a route.
- **`route-add-route <parent_route> <child_route>`** - Add a route to another route.
- **`route-remove <route> <stop_name>`** - Remove a stop from a route.

- **`stop-create <name>`** - Creates a stop with the provided name.
- **`stop-delete <name>`** - Deletes a stop with that name.

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
