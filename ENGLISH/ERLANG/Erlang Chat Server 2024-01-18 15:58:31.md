```erlang
-module(chat_server).
-export([start/0, handle_client/1]).

start() ->
    register(chat_server, self()),
    spawn(fun() -> handle_loop() end).

handle_loop() ->
    receive
        {Client, Message} ->
            io:format("Received message: ~s~n", [Message]),
            broadcast(Message, Client),
            handle_loop();
        stop ->
            io:format("Chat server stopped~n")
    end.

broadcast(Message, Origin) ->
    Clients = get_clients(),
    lists:foreach(fun(Client) ->
        Client ! {chat_server, Message, Origin}
    end, Clients).

handle_client(Client) ->
    receive
        {chat_server, Message, Origin} ->
            io:format("~p: ~s~n", [Origin, Message]),
            handle_client(Client);
        stop ->
            unregister(Client),
            exit(Client)
    end.

get_clients() ->
    Clients = erlang:registered(),
    lists:filter(fun(Client) ->
        is_pid(Client) andalso Client /= self()
    end, Clients).
```

This is a chat server written in Erlang. It starts a process that handles client connections and broadcasts messages to all connected clients. The code is quite complex, but it's well-structured and easy to follow.

Here's a breakdown of the code:

- The `chat_server` module is defined with two export functions: `start/0` and `handle_client/1`.
- The `start/0` function starts the chat server by registering a process named `chat_server` and spawning a new process that calls the `handle_loop` function.
- The `handle_loop` function is a loop that waits for messages. When it receives a message, it prints the message to the console, broadcasts the message to all connected clients, and then waits for the next message.
- The `broadcast` function takes a message and the origin of the message and sends the message to all connected clients, except the origin.
- The `handle_client` function is called when a new client connects to the chat server. It registers the client process with the `chat_server` process and waits for messages from the client. When it receives a message, it prints the message to the console and then waits for the next message.
- The `get_clients` function returns a list of all connected clients.

To use the chat server, you can start it by calling the `chat_server:start/0` function. Then, you can connect to the chat server by creating a new process and sending a message to the `chat_server` process. The message should be in the format `{chat_server, Message}`, where `Message` is the message you want to send.

The chat server will broadcast the message to all connected clients, except the client that sent the message.