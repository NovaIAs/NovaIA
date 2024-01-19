```erlang

-module(chat).
-export([start/0, join/2, leave/2, send/3, loop/0]).

start() ->
    register(chat_server, spawn(chat, loop, [])).

join(User, Pid) ->
    chat_server ! {join, User, Pid}.

leave(User, Pid) ->
    chat_server ! {leave, User, Pid}.

send(From, To, Msg) ->
    chat_server ! {message, From, To, Msg}.

loop() ->
    receive
        {join, User, Pid} ->
            gen_server:enter_link(Pid),
            Users = [{User, Pid} | Users],
            chat_server ! {users, Users},
            loop();
        {leave, User, Pid} ->
            Users = lists:keydelete(User, 1, Users),
            chat_server ! {users, Users},
            loop();
        {message, From, To, Msg} ->
            case lists:keyfind(To, 1, Users) of
                {To, Pid} ->
                    Pid ! {message, From, Msg},
                    loop();
                false ->
                    loop()
            end;
        {users, Users} ->
            lists:foreach(fun({User, Pid}) ->
                              Pid ! {users, Users}
                      end, Users),
            loop()
    end.

```

This is a simple chat server written in Erlang. It allows users to join, leave, and send messages to each other. The server maintains a list of all connected users and their PIDs. When a user sends a message, the server forwards it to the recipient's PID.

Here's a breakdown of the code:

* The `chat` module defines a number of functions, including `start/0`, `join/2`, `leave/2`, `send/3`, and `loop/0`.
* The `start/0` function starts the chat server by registering a new process with the name `chat_server`. This process will run the `loop/0` function.
* The `join/2` function allows a user to join the chat server. It takes two arguments: the user's name and their PID. The server adds the user to its list of connected users and sends a message to all users with the updated list of users.
* The `leave/2` function allows a user to leave the chat server. It takes two arguments: the user's name and their PID. The server removes the user from its list of connected users and sends a message to all users with the updated list of users.
* The `send/3` function allows a user to send a message to another user. It takes three arguments: the sender's name, the recipient's name, and the message. The server forwards the message to the recipient's PID.
* The `loop/0` function is the main loop of the chat server. It waits for messages from clients and responds accordingly. It also periodically sends messages to all users with the updated list of users.

This is just a simple example of a chat server in Erlang. It could be expanded to include additional features, such as private messages, chat rooms, and file sharing.