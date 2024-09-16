module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [ "<system>"
    , "<command>"
    , "<create_command>"
    , "<update_command>"
    , "<retrieve_command>"
    , "<delete_command>"
    , "<compound_command>"
    , "<routes>"
    , "<route_list>"
    , "<route>"
    , "<bus_list>"
    , "<bus>"
    , "<departure_time_list>"
    , "<departure_time>"
    , "<stop_list>"
    , "<stop>"
    , "<time_list>"
    , "<route_id>"
    , "<route_name>"
    , "<bus_id>"
    , "<bus_number>"
    , "<start_time>"
    , "<end_time>"
    , "<stop_id>"
    , "<stop_name>"
    , "<time>"
    , "<string>"
    , "<char>"
    ]