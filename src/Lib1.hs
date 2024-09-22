module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [
      "route_list"
    , "route_list_internal"
    , "route"
    , "route_id"
    , "stop_list"
    , "nested_route_list"
    , "stop"
    , "stop_id"
    , "name"
    , "char" 
    ]