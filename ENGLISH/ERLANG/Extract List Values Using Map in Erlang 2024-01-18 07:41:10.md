```erlang
-module(duplicate_with_map).
-compile(export_all).

duplicate_with_map(List) ->
  Map = maps:from_list([{X,Y} || {X,Y} <- List]),
  P1 = proplists:get_value(a, Map, 0),
  P2 = proplists:get_value(b, Map, 0),
  P3 = proplists:get_value(c, Map, 0),
  [P1, P2, P3].

```

This code defines an Erlang function called `duplicate_with_map`. This function takes a list as an argument and returns a new list with three elements.

The function first converts the input list to a map using the `maps:from_list/1` function. This creates a map where the keys are the elements of the input list and the values are the elements themselves.

Next, the function uses the `proplists:get_value/3` function to retrieve the values for the keys "a", "b", and "c" from the map. If any of these keys are not found in the map, the default value of 0 is used.

Finally, the function returns a list containing the values for the keys "a", "b", and "c".

For example, if you call the `duplicate_with_map` function with the list `[{"a", 1}, {"b", 2}, {"c", 3}]`, it will return the list `[1, 2, 3]`.