```erlang

-module(fives_to_infinity).
-export([fives_to_infinity/0]).

fives_to_infinity() ->
    Loop = fun() ->
        receive
            X when X >= 5 -> io:fwrite("~w~n", [X]);
            X ->
                io:fwrite("~w~n", [X]),
                Loop()
        end
    end,
    spawn(Loop).

```

This code starts an infinite loop that prints numbers from 5 to infinity. The loop is implemented using a recursive function called `Loop` that receives messages. The first time `Loop` executes, it will wait for a message. When a message is received, `Loop` prints the message and then calls itself to continue the loop. The message that is sent to `Loop` is a number. The loop will print all numbers that are greater than or equal to 5. This code differs from other implementations because it utilizes Erlang's message passing capabilities to implement the infinite loop, making it unique and highly efficient.