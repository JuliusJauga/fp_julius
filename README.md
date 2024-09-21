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

- **create_route**: Adds a new route to the system.
    - Parameters: `route_id`, `route_name`

- **create_stop**: Adds a new stop to a bus route.
    - Parameters: `stop_id`, `stop_name`

- **create_sub_route**: Adds a subroute to an existing route
    - Parameters: `parent_route_id`, `sub_route_id`, `sub_route_name`

- **update_route**: Updates an existing route.
    - Parameters: `route_id`, `route_name`, `route_list`, `stops`

- **update_stop**: Updates an existing stop.
    - Parameters: `stop_id`, `stop_name`

- **update_sub_route**: Updates an existing sub-route.
    - Parameters: `parent_route_id`, `route_id`, `route_name`, `route_list`, `stops`

- **get_route**: Retrieves information about a route.
    - Parameters: `route_id`

- **get_sub_routes**: Retrieves a list of routes, that make a bigger route.
    - Parameters: `parent_route_id`

- **delete_route**: Deletes a route from the system.
    - Parameters: `route_id`

- **delete_stop**: Deletes a stop from a route.
    - Parameters: `stop_id`

- **delete_sub_route**: Deletes a sub-route from the system.
    - Parameters: `parent_route_id`, `sub_route_id`

- **get_routes_from_stop**: Retrieves routes that have a specific stop.
    - Parameters: `stop_id`

### Recursion

Recursion is in routes being made of other routes, each having their stops with departure times.

See fp_julius/grammar.txt

### Basic structure

```
routes 
    route_id
    route_name
    routes
        route_id
        route_name
        routes
            route_id
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
        stop_id
        stop_name
    ...
```
A route can be made of other smaller routes, each having their own stops.

# Example

```
<route_id 2F route_name route1 routes 
    <
    route_id 2D route_name route2 routes 
    <
        route_id 2C route_name route3 routes stops 
        <
            stop_id 1 stop_name Pirmoji
        > 
        route_id 2B route_name route4 routes stops 
        <
            stop_id 2 stop_name Antroji
        >
    >
    stops
    <
    stop_id 1 stop_name Pirmoji
    stop_id 2 stop_name Antroji
    stop_id 3 stop_name Trecioji
    > 
    stops
    <
    stop_id 1 stop_name Pirmoji
    stop_id 2 stop_name Antroji
    stop_id 3 stop_name Trecioji
    stop_id 4 stop_name Ketvirtoji
    >
>
```
