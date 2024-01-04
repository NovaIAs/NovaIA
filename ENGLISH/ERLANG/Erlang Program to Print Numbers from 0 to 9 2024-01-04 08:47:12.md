```erlang
-module(my_module).

-export([start/0, loop/1]).

start() ->
    loop(0).

loop(N) ->
    io:fwrite("Iteration ~p~n", [N]),
    if
        N < 10 ->
            loop(N + 1);
        true ->
            exit(normal)
    end.
```

This code is a simple Erlang program that prints the numbers from 0 to 9, one per line. The start() function starts the program by calling the loop() function with the initial value of 0. The loop() function prints the current iteration number and then recursively calls itself with the next iteration number. The loop() function exits when the iteration number reaches 10.

The following is a more detailed explanation of the code:

* The -module(my_module) directive specifies the name of the Erlang module.
* The -export([start/0, loop/1]) directive exports the start() and loop() functions from the module. This means that these functions can be called from other modules.
* The start() function is the entry point of the program. It calls the loop() function with the initial value of 0.
* The loop() function is a recursive function that prints the current iteration number and then recursively calls itself with the next iteration number.
* The if statement in the loop() function checks if the iteration number is less than 10. If it is, the function recursively calls itself with the next iteration number. If it is not, the function exits with the exit(normal) call.

This code is a simple example of how to write a recursive function in Erlang. It can be used to solve a variety of problems, such as finding the factorial of a number or calculating the Fibonacci sequence.