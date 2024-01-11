```erlang
-module(hello_world).
-export([start/0, loop/0]).

start() ->
    spawn(hello_world, loop, []).

loop() ->
    receive
        {From, "hello"} ->
            From ! "world",
            loop();
        {From, "exit"} ->
            exit(From, killed)
    end.
```

This code defines an Erlang module named hello_world that implements a simple echo server. Here's how it works:

1. The -module(hello_world) line specifies the name of the module.

2. The -export([start/0, loop/0]) line exports two functions, start/0 and loop/0, from this module. This means that these functions can be called from other modules.

3. The start/0 function is a simple Erlang function that creates a new process by calling the spawn function. The spawn function takes three arguments:

    - The name of the module containing the function to be executed in the new process (hello_world in this case).
    - The name of the function to be executed in the new process (loop in this case).
    - A list of arguments to be passed to the function (an empty list in this case).

4. The loop/0 function is an infinite loop that uses the receive construct to wait for messages. The receive construct is used to receive messages from other processes. In this case, the loop function can receive two types of messages:

    - A message containing the atom "hello" from another process.
    - A message containing the atom "exit" from another process.

5. When the loop function receives a message containing the atom "hello", it sends a message containing the atom "world" back to the sender of the "hello" message. Then, it calls itself recursively to continue waiting for messages.

6. When the loop function receives a message containing the atom "exit", it exits the current process by calling the exit function. The exit function takes two arguments:

    - The process ID of the process to be exited.
    - A reason for exiting the process (killed in this case).

7. To start the echo server, you can call the start/0 function from the Erlang shell. This will create a new process that will wait for messages.

8. To send a message to the echo server, you can use the ! operator to send a message to a process. For example, you can send a message containing the atom "hello" to the echo server by using the following command in the Erlang shell:

```
1> hello_world ! {self(), "hello"}.
```

9. When the echo server receives the "hello" message, it will send a message containing the atom "world" back to the sender of the "hello" message. You can receive this message in the Erlang shell by using the receive construct. For example, you can receive the "world" message in the Erlang shell by using the following command:

```
2> receive
     {From, "world"} ->
         io:format("Received ~p from ~p~n", [From, self()])
 end.
```