Claro! Aqui está um exemplo de um código em Erlang que cria um servidor para um jogo multiplayer simples:

```erlang
-module(server).
-export([start/0, loop/0, handle_client/2]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(9999, [binary, {active, false}]),
    io:format("Servidor iniciado na porta 9999~n"),
    loop(ListenSocket).

loop(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(ClientSocket, ListenSocket) end),
    loop(ListenSocket).

handle_client(ClientSocket, ListenSocket) ->
    inet:setopts(ClientSocket, [{active, once}]),
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
            io:format("Recebido: ~p~n", [Data]),
            handle_data(Data, ClientSocket),
            handle_client(ClientSocket, ListenSocket);
        {error, closed} ->
            gen_tcp:close(ClientSocket),
            io:format("Cliente desconectado~n");
        _ ->
            handle_client(ClientSocket, ListenSocket)
    end.

handle_data(Data, ClientSocket) ->
    % Lógica do jogo aqui
    Response = process_data(Data),
    gen_tcp:send(ClientSocket, Response).

process_data(Data) ->
    % Lógica do jogo aqui
    Response = "Resposta do servidor",
    Response.
```

Neste código, criamos um servidor TCP simples em Erlang que aceita conexões de clientes e processa os dados recebidos. O servidor escuta na porta 9999 e, quando um cliente se conecta, ele cria um processo para lidar com essa conexão.

A função `start/0` é responsável por iniciar o servidor. Ela cria o socket de escuta e chama a função `loop/0` passando o socket como argumento.

A função `loop/1` é um loop infinito que espera por novas conexões de clientes. Quando uma conexão é estabelecida, a função `handle_client/2` é chamada em um processo separado para lidar com essa conexão.

A função `handle_client/2` configura o socket do cliente para receber dados em modo ativo e, em seguida, verifica se há dados recebidos. Se houver dados, eles são processados pela função `handle_data/2` e uma resposta é enviada de volta para o cliente. Se ocorrer um erro ou o cliente desconectar, o socket do cliente é fechado.

A função `handle_data/2` é onde a lógica do jogo deve ser implementada. Neste exemplo, ela simplesmente retorna uma resposta fixa.

Este é apenas um exemplo básico de um servidor de jogo em Erlang. Você pode adicionar mais funcionalidades, como suporte a vários jogadores, armazenamento de dados de jogo, etc.