```erlang
-module(dynamic_dictionary).
-export([new/0, insert/3, lookup/2, delete/2]).

-opaque dictionary() :: term().

-spec new() -> dictionary().
new() ->
    orddict:new().

-spec insert(Key :: term(), Value :: term(), Dict :: dictionary()) -> dictionary().
insert(Key, Value, Dict) ->
    orddict:store(Key, Value, Dict).

-spec lookup(Key :: term(), Dict :: dictionary()) -> {ok, Value :: term()} | {error, not_found}.
lookup(Key, Dict) ->
    case orddict:find(Key, Dict) of
        {ok, Value} -> {ok, Value};
        error -> {error, not_found}
    end.

-spec delete(Key :: term(), Dict :: dictionary()) -> dictionary().
delete(Key, Dict) ->
    orddict:erase(Key, Dict).
```

Explanation:

This code defines a module called `dynamic_dictionary` that implements a dynamic dictionary data structure in Erlang. A dictionary is a data structure that stores key-value pairs, and this implementation uses the Erlang `orddict` module to manage the dictionary.

1. `new/0`: This function creates a new empty dictionary. It calls the `orddict:new()` function, which returns a new ordered dictionary.

2. `insert/3`: This function inserts a new key-value pair into the dictionary. It takes three arguments: `Key`, `Value`, and `Dict`. The `Key` is the key to be inserted, the `Value` is the value to be associated with the key, and `Dict` is the dictionary to insert the key-value pair into. The function calls the `orddict:store/3` function, which inserts the key-value pair into the dictionary and returns the updated dictionary.

3. `lookup/2`: This function looks up the value associated with a given key in the dictionary. It takes two arguments: `Key` and `Dict`. The `Key` is the key to look up, and `Dict` is the dictionary to look up the key in. The function calls the `orddict:find/2` function, which returns either `{ok, Value}` if the key is found or `error` if the key is not found.

4. `delete/2`: This function deletes a key-value pair from the dictionary. It takes two arguments: `Key` and `Dict`. The `Key` is the key to delete, and `Dict` is the dictionary to delete the key from. The function calls the `orddict:erase/2` function, which deletes the key-value pair from the dictionary and returns the updated dictionary.

This dynamic dictionary implementation is useful for storing and retrieving data in a structured way. It is commonly used in Erlang applications for various purposes, such as configuration management, data caching, and state management.