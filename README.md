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

- **update_route**: Updates an existing route.
    - Parameters: `route_id`, `route_name`, `bus_list`

- **update_bus**: Updates an existing bus.
    - Parameters: `bus_id`, `bus_number`, `departure_time_list`, `stop_list`

- **update_stop**: Updates an existing stop.
    - Parameters: `stop_id`, `stop_name`, `time_list`

- **get_route**: Retrieves information about a route.
    - Parameters: `route_id`

- **get_bus**: Retrieves information about a bus.
    - Parameters: `bus_id`

- **get_stop**: Retrieves information about a stop.
    - Parameters: `stop_id`

- **delete_route**: Deletes a route from the system.
    - Parameters: `route_id`

- **delete_bus**: Deletes a bus from a route.
    - Parameters: `bus_id`

- **delete_stop**: Deletes a stop from a bus route.
    - Parameters: `stop_id`

- **get-routes-from-stop**: Retrieves routes that have a specific stop.
    - Parameters: `stop_id`

### Recursion

A recursion example would be a compound command.

A compound command is a command that consists of two or more commands, some of which can also be compound commands, making it recursive. 

See fp_julius/grammar.txt

### Basic structure

```
routes 
    route_id
    route_name
    buses 
        bus_id
        bus_number
        departure_times 
            start_time
            end_time
        stops 
            stop_id
            stop_name
            bus_departure_times 
                time
```
A route may have buses, buses may have stops and stops may have departure times. Each of the blocks may have data for distinguishing between them (name, id).