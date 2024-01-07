```erlang
-module(complex_erlang_code).

-export([
  start/0,
  do_something/1,
  do_something_else/2,
  handle_message/2
]).

start() ->
  Pid = spawn_link(fun() -> handle_message() end),
  Pid ! {self(), hello},
  receive
    {Pid, goodbye} ->
      io:fwrite("Got goodbye from ~p~n", [Pid]),
      ok
  end.

do_something(X) ->
  {X, X + 1}.

do_something_else(X, Y) ->
  X + Y.

handle_message() ->
  receive
    {From, hello} ->
      io:fwrite("Got hello from ~p~n", [From]),
      From ! {self(), goodbye},
      handle_message();
    {From, goodbye} ->
      io:fwrite("Got goodbye from ~p~n", [From])
  end.
```

This code defines an Erlang module called `complex_erlang_code`.

The module exports four functions:

* `start/0`: This function starts a loop that waits for messages.
* `do_something/1`: This function takes an argument and returns a tuple containing the argument and the argument plus one.
* `do_something_else/2`: This function takes two arguments and returns the sum of the arguments.
* `handle_message/2`: This function handles messages sent to the process that started the loop in the `start/0` function.

The `start/0` function calls the `spawn_link` function to create a new process that will handle messages. The `spawn_link` function returns the process ID (PID) of the new process.

The `start/0` function then sends a message to the new process using the `!` operator. The message is a tuple containing the PID of the calling process and the atom `hello`.

The `handle_message/2` function waits for messages to arrive. When a message arrives, the `handle_message/2` function prints the message to the console using the `io:fwrite` function.

If the message is a tuple containing the atom `hello`, the `handle_message/2` function sends a message back to the calling process using the `!` operator. The message is a tuple containing the PID of the current process and the atom `goodbye`.

If the message is a tuple containing the atom `goodbye`, the `handle_message/2` function prints the message to the console using the `io:fwrite` function.

The `do_something/1` and `do_something_else/2` functions are simple functions that perform some calculations and return a result.

To use the code, you can start the loop by calling the `start/0` function. You can then send messages to the loop by calling the `!` operator with the PID of the loop and a message.