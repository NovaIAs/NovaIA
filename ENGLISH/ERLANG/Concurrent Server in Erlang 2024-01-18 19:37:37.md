```erlang
-module(turing).

-export([loop/0]).

loop() ->
    receive
        {X, Request} ->
            try
                Response = X:Request(receive
                                            N -> from(N)
                                        end),
                loop()!{X, Response}
            catch
                X:badarg ->
                    loop()!{X, {badarg, error}}
            end;
        {X, Reply} ->
            X!Reply,
            loop()
    end.

to(Pid, Request) ->
    loop()!{Pid, Request},
    receive
        {Pid, Response} -> Response
    end.

from(Pid) ->
    receive
        Message ->
            Pid!{ok, Message},
            from(Pid)
    end.
```

This is a simple concurrent server in Erlang.
It consists of two modules: `turing` and `to`.
The `turing` module contains the main loop of the server, which continuously receives requests from clients and forwards them to the appropriate service process.
It also receives responses from service processes and forwards them back to the clients.
The `to` module is used by clients to send requests to the server and receive responses.

The main loop of the server is implemented in the `loop/0` function.
This function uses the `receive` statement to wait for messages from clients or service processes.
When a message is received, the function extracts the sender process ID (`X`) and the request or response message (`Request` or `Reply`).
If the message is a request, the function forwards it to the appropriate service process using the `X:Request(receive N -> from(N) end)` expression.
This expression creates a new process that executes the `Request` function in the context of the service process `X`.
The `receive N -> from(N) end` expression is used to create a channel that the service process can use to send responses back to the main loop.
If the message is a response, the function forwards it back to the client process using the `X!Reply` expression.

The `to(Pid, Request)` function is used by clients to send requests to the server.
This function sends the request message to the main loop of the server using the `loop()!{Pid, Request}` expression.
It then waits for a response message from the server using the `receive {Pid, Response} -> Response end` expression.

The `from(Pid)` function is used by service processes to send responses back to the main loop of the server.
This function uses the `receive Message -> Pid!{ok, Message}, from(Pid) end` expression to wait for a message from the client process.
When a message is received, the function sends it back to the main loop using the `Pid!{ok, Message}` expression and then continues waiting for the next message.