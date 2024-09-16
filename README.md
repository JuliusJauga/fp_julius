# fp_julius
1 laboratory work.

## Domain 

City Public Transport Schedule System

This system helps manage and organize the public transport routes and stops in a city. It allows you to add, remove, and list routes, buses, and stops. Additionally, it provides functionalities to manage departure time and retrieve information about routes, buses, and stops.

## BNF
The grammar for this domain is provided in fp_julius/grammar.txt.

It is written in BNF.

BNF tested on https://bnfplayground.pauliankline.com/
## Commands

- **create_route**: Adds a new route to the system.
    - Parameters: `route_id`, `route_name`, `bus_list`

- **create_bus**: Adds a new bus to a route.
    - Parameters: `bus_id`, `bus_number`, `departure_time_list`, `stop_list`

- **create_stop**: Adds a new stop to a bus route.
    - Parameters: `stop_id`, `stop_name`, `time_list`

- **create_sub_route**: Adds a subroute to an existing route
    - Parameters: `parent_route_id`, `sub_route_id`, `sub_route_name`, `bus_list`

- **create_sub_stop**: Creates another stop, going to another direction in a stop, like a station with different platforms.
    - Parameters: `stop_id`, `sub_stop_id`, `sub_stop_name`, `time_list`

- **update_route**: Updates an existing route.
    - Parameters: `route_id`, `route_name`, `bus_list`

- **update_bus**: Updates an existing bus.
    - Parameters: `bus_id`, `bus_number`, `departure_time_list`, `stop_list`

- **update_stop**: Updates an existing stop.
    - Parameters: `stop_id`, `stop_name`, `time_list`

- **update_sub_route**: Updates an existing sub-route.
    - Parameters: `parent_route_id`, `route_id`, `route_name`, `bus_list`

- **update_sub_stop**: Updates an existing sub-stop.
    - Parameters: `parent_stop_id`, `sub_stop_id`, `sub_stop_name`,`time_list`

- **get_route**: Retrieves information about a route.
    - Parameters: `route_id`

- **get_sub_routes**: Retrieves a list of routes, that make a bigger route.
    - Parameters: `parent_route_id`

- **get_bus**: Retrieves information about a bus.
    - Parameters: `bus_id`

- **get_stop**: Retrieves information about a stop.
    - Parameters: `stop_id`

- **get_sub_stops**: Retrieves a list of sub-stops, that make a stop.
    - Parameters: `parent_stop_id`

- **delete_route**: Deletes a route from the system.
    - Parameters: `route_id`

- **delete_bus**: Deletes a bus from a route.
    - Parameters: `bus_id`

- **delete_stop**: Deletes a stop from a bus route.
    - Parameters: `stop_id`

- **delete_sub_route**: Deletes a sub-route from the system.
    - Parameters: `parent_route_id`, `sub_route_id`

- **delete_sub_stop**: Deletes a sub-stop from a stop.
    - Parameters: `parent_stop_id`, `sub_stop_id`

- **get_routes_from_stop**: Retrieves routes that have a specific stop.
    - Parameters: `stop_id`

- **compound_command**: Executes a series of commands in sequence.
    - Parameters: `command1`, `command2`, ...


### Recursion

A recursion example would be a compound command.

A compound command is a command that consists of two or more commands, some of which can also be compound commands, making it recursive. 

Recursion could be in routes consisting of other routes, or stops that are made of one or more stops.

See fp_julius/grammar.txt

### Basic structure

```
routes 
    route_id
    route_name
    routes
        ...
    buses 
        bus_id
        bus_number
        departure_times 
            start_time
            end_time
        stops 
            stop_id
            stop_name
            stops
                ...
            bus_departure_times 
                time
            
```
A route may have buses, buses may have stops and stops may have departure times. Each of the blocks may have data for distinguishing between them (name, id).

A route can be made of other smaller routes, each having their own schedules and buses.

A stop can be made of other stops / platforms, going to different directions.