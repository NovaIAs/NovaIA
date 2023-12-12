Claro! Aqui está um exemplo de um código complexo em Erlang que envolve a criação de uma aplicação de chat em tempo real com o uso de sockets:

```erlang
-module(chat_server).
-export([start/0, stop/0, handle_request/2]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, raw}, {active, false}]),
    spawn(fun() -> accept_connections(ListenSocket) end).

stop() ->
    gen_tcp:close(ListenSocket).

accept_connections(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(ClientSocket) end),
    accept_connections(ListenSocket).

handle_client(ClientSocket) ->
    loop(ClientSocket).

loop(ClientSocket) ->
    receive
        {tcp, ClientSocket, Data} ->
            handle_request(Data, ClientSocket),
            loop(ClientSocket);
        {tcp_closed, ClientSocket} ->
            ok
    end.

handle_request(Data, ClientSocket) ->
    {ok, _} = gen_tcp:send(ClientSocket, Data).
```

Neste exemplo, temos um módulo chamado `chat_server` que contém três funções principais: `start`, `stop` e `handle_request`. 

A função `start` é responsável por iniciar o servidor de chat. Aqui, estamos utilizando a função `gen_tcp:listen` para criar um socket TCP que escuta na porta 1234. Em seguida, usamos a função `spawn` para criar um processo que irá lidar com as conexões recebidas.

A função `stop` é responsável por fechar o socket do servidor.

A função `accept_connections` é utilizada para aceitar novas conexões de clientes. Aqui, usamos a função `gen_tcp:accept` para aceitar uma conexão e criar um novo socket para o cliente. Em seguida, utilizamos a função `spawn` para criar um novo processo que irá lidar com o cliente.

A função `handle_client` é responsável por lidar com as mensagens recebidas de um cliente específico. Aqui, temos um loop infinito que recebe mensagens do cliente através do socket TCP. Em seguida, utiliza a função `handle_request` para processar a mensagem recebida e enviar uma resposta de volta para o cliente.

A função `loop` é responsável por manter o loop de comunicação com o cliente. Ela recebe mensagens através do socket TCP e chama a função `handle_request` para processar cada mensagem.

A função `handle_request` é responsável por processar a mensagem recebida do cliente. Neste exemplo, estamos simplesmente enviando a mensagem de volta para o cliente utilizando a função `gen_tcp:send`.

Esse é apenas um exemplo básico de um servidor de chat em tempo real em Erlang. Existem muitas outras funcionalidades que podem ser adicionadas, como autenticação de usuários, armazenamento de mensagens em banco de dados, entre outros.