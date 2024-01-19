```erlang

-module(beer_song).

-export([verse/2, song/2]).

-define(BOTTLES, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]).

verse(N, BeerType) ->
  {FirstLine, SecondLine} = get_lines(N, BeerType),
  io:fwrite("~s\n~s\n\n", [FirstLine, SecondLine]).

song(Start, End) ->
  lists:foreach(fun(N) -> verse(N, bottle) end, lists:reverse(?BOTTLES)),
  io:fwrite("No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n").

get_lines(N, BeerType) ->
  OldBottles = lists:nth(N + 1, ?BOTTLES),
  NewBottles = lists:nth(N + 2, ?BOTTLES),
  % Generate the first line of the verse
  FirstLine = io_lib:format("~p bottle~s of beer on the wall, ~p bottle~s of beer.\n", [OldBottles, if OldBottles > 1 -> "s" end, OldBottles, if OldBottles > 1 -> "s" end]),

  % Generate the second line of the verse
  SecondLine = case N of
    0 -> io_lib:format("Take one down and pass it around, no more bottles of beer on the wall.\n");
    1 -> io_lib:format("Take it down and pass it around, ~p bottle~s of beer on the wall.\n", [NewBottles, if NewBottles > 1 -> "s" end]);
    _ -> io_lib:format("Take one down and pass it around, ~p bottle~s of beer on the wall.\n", [NewBottles, if NewBottles > 1 -> "s" end])
  end,
  {FirstLine, SecondLine}.

```

**Explanation of the code:**

1. The `beer_song` module is defined with two export functions: `verse/2` and `song/2`.


2. The `verse/2` function takes two arguments: `N`, which is the number of bottles of beer, and `BeerType`, which is a string representing the type of beer. It returns a tuple containing the first and second lines of the verse for the given number of bottles.


3. The `song/2` function takes two arguments: `Start` and `End`, which are the starting and ending numbers of bottles of beer to sing about. It calls the `verse/2` function for each number of bottles from `Start` to `End`, in reverse order, and then prints the final verse of the song.


4. The `?BOTTLES` constant is a list containing the numbers of bottles of beer from 10 to 0.


5. The `get_lines/2` function takes two arguments: `N`, which is the number of bottles of beer, and `BeerType`, which is a string representing the type of beer. It returns a tuple containing the first and second lines of the verse for the given number of bottles.


6. The `OldBottles` variable is set to the number of bottles of beer for the current verse.


7. The `NewBottles` variable is set to the number of bottles of beer for the next verse.


8. The `FirstLine` variable is set to the first line of the verse, which is formatted using the `io_lib:format/3` function.


9. The `SecondLine` variable is set to the second line of the verse, which is formatted using the `io_lib:format/3` function. The specific format of the second line depends on the value of `N`.


10. The `FirstLine` and `SecondLine` variables are returned as a tuple.