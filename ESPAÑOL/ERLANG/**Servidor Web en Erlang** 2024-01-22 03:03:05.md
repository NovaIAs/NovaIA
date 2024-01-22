```erlang
% Este módulo implementa un servidor web sencillo en Erlang.

-module(servidor_web).

-export([start/0, stop/0]).

% Iniciar el servidor web.
start() ->
    Port = 8080,
    ok = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true},
        {active, true}]),
    spawn(fun() -> accept_loop(Port) end).

% Detener el servidor web.
stop() ->
    ok.

% Bucle de aceptación de conexiones.
accept_loop(Port) ->
    {ok, Socket} = gen_tcp:accept(Port),
    spawn(fun() -> handle_connection(Socket) end),
    accept_loop(Port).

% Manejar una conexión con un cliente.
handle_connection(Socket) ->
    try
        Request = recv_request(Socket),
        Response = handle_request(Request),
        send_response(Socket, Response)
    catch
        _:_ -> close_connection(Socket)
    end.

% Recibir una petición HTTP.
recv_request(Socket) ->
    Request = gen_tcp:recv(Socket, 0),
    {ok, Request}.

% Manejar una petición HTTP.
handle_request(Request) ->
    case string:tokens(Request, "\r\n") of
        ["GET / HTTP/1.1", _] -> "HTTP/1.1 200 OK\r\n\r\nHello, world!";
        _ -> "HTTP/1.1 400 Bad Request\r\n\r\nBad request"
    end.

% Enviar una respuesta HTTP.
send_response(Socket, Response) ->
    gen_tcp:send(Socket, Response).

% Cerrar una conexión.
close_connection(Socket) ->
    gen_tcp:close(Socket).
```

Este código implementa un servidor web sencillo en Erlang. El servidor escucha en el puerto 8080 y acepta conexiones de clientes. Cuando se recibe una conexión, el servidor genera un proceso para manejar la conexión. El proceso de conexión recibe la petición HTTP del cliente, la maneja y devuelve una respuesta HTTP. El proceso de conexión se cierra cuando el cliente se desconecta.

El código está dividido en varios módulos. El módulo `servidor_web` contiene las funciones para iniciar y detener el servidor web. El módulo `conexion` contiene las funciones para manejar una conexión con un cliente.

El código está escrito en Erlang, un lenguaje de programación concurrente y funcional. Erlang es un lenguaje muy potente y eficiente, que se utiliza a menudo para desarrollar sistemas distribuidos y en tiempo real.