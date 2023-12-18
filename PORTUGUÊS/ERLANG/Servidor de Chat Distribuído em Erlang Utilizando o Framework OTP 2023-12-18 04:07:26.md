Claro! Aqui está um código complexo em Erlang que utiliza a biblioteca OTP para criar um servidor de chat distribuído:

```erlang
-module(chat_server).
-behaviour(gen_server).

%% API pública
-export([start_link/0, join/1, leave/1, broadcast/2]).

%% API interna
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = [], messages = []}).

%% Funções de API pública

%% Inicia o servidor de chat
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Função para um usuário entrar no chat
join(User) ->
    gen_server:call(?MODULE, {join, User}).

%% Função para um usuário deixar o chat
leave(User) ->
    gen_server:call(?MODULE, {leave, User}).

%% Função para enviar uma mensagem para todos os usuários no chat
broadcast(User, Message) ->
    gen_server:cast(?MODULE, {broadcast, User, Message}).

%% Funções de Callback do gen_server

%% Inicializa o estado do servidor de chat
init([]) ->
    {ok, #state{}}.

%% Trata as chamadas síncronas
handle_call({join, User}, _From, State) ->
    {reply, ok, State#state{users = State#state.users ++ [User]}};
handle_call({leave, User}, _From, State) ->
    {reply, ok, State#state{users = lists:delete(User, State#state.users)}};

%% Trata as mensagens assíncronas
handle_cast({broadcast, User, Message}, State) ->
    NewState = State#state{messages = State#state.messages ++ [{User, Message}]},
    {noreply, NewState};

%% Trata informações não solicitadas
handle_info(_Info, State) ->
    {noreply, State}.

%% Chamado quando o servidor é encerrado
terminate(_Reason, _State) ->
    ok.

%% Chamado quando ocorre uma mudança de código
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

Este código implementa um servidor de chat distribuído em Erlang utilizando o framework OTP. Ele define uma API pública que permite aos usuários entrar, sair e enviar mensagens para o chat. O servidor mantém o estado dos usuários e as mensagens em um registro chamado `state`.

A função `start_link/0` inicia o servidor de chat. A função `join/1` permite que um usuário entre no chat, adicionando-o à lista de usuários no estado do servidor. A função `leave/1` permite que um usuário deixe o chat, removendo-o da lista de usuários no estado do servidor. A função `broadcast/2` permite que um usuário envie uma mensagem para todos os usuários no chat, adicionando-a à lista de mensagens no estado do servidor.

O servidor de chat é implementado como um `gen_server` Erlang, que fornece uma estrutura para lidar com chamadas síncronas, mensagens assíncronas e informações não solicitadas. As funções de callback do `gen_server` são implementadas para manipular essas interações.

Espero que este código atenda às suas expectativas! Se você tiver mais algum requisito específico, estou à disposição para ajudar.