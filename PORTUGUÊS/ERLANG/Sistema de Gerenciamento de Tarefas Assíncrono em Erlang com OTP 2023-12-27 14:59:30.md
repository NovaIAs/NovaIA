Claro! Aqui está um exemplo de código complexo em Erlang, que utiliza a biblioteca OTP para criar um sistema de gerenciamento de tarefas assíncrono:

```erlang
-module(task_manager).
-behaviour(gen_server).

%% API pública
-export([start/0, add_task/1, get_task/1, complete_task/1]).

%% API interna
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tasks = []}).

%% API pública

%% Inicia o servidor
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Adiciona uma nova tarefa ao sistema
add_task(Task) ->
    gen_server:call(?MODULE, {add_task, Task}).

%% Obtém uma tarefa pelo seu ID
get_task(TaskId) ->
    gen_server:call(?MODULE, {get_task, TaskId}).

%% Marca uma tarefa como concluída
complete_task(TaskId) ->
    gen_server:call(?MODULE, {complete_task, TaskId}).

%% API interna

%% Inicializa o servidor
init([]) ->
    {ok, #state{}}.

%% Trata chamadas síncronas
handle_call({add_task, Task}, _From, State) ->
    NewTaskId = generate_task_id(),
    NewTask = {NewTaskId, Task},
    NewState = State#state{tasks = [NewTask | State#state.tasks]},
    {reply, {ok, NewTaskId}, NewState};

handle_call({get_task, TaskId}, _From, State) ->
    {reply, find_task(TaskId, State), State};

handle_call({complete_task, TaskId}, _From, State) ->
    NewState = State#state{tasks = complete_task(TaskId, State)},
    {reply, ok, NewState}.

%% Trata chamadas assíncronas
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Trata mensagens de sistema
handle_info(_Info, State) ->
    {noreply, State}.

%% Trata terminação
terminate(_Reason, _State) ->
    ok.

%% Lida com alterações no código
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Funções auxiliares

%% Gera um ID único para cada tarefa
generate_task_id() ->
    {_, Time} = now(),
    random:uniform(100000) + Time.

%% Encontra uma tarefa pelo ID
find_task(TaskId, State) ->
    lists:keyfind(TaskId, #1, State#state.tasks).

%% Marca uma tarefa como concluída
complete_task(TaskId, State) ->
    lists:keydelete(TaskId, #1, State#state.tasks).
```

Este código é um exemplo de um sistema de gerenciamento de tarefas assíncrono em Erlang. É escrito utilizando o comportamento `gen_server`, que permite criar servidores de processos com um conjunto de funções bem definidas.

O módulo `task_manager` implementa uma API pública para adicionar, obter e marcar tarefas como concluídas. Internamente, ele utiliza um registro de estado para armazenar as tarefas. Cada tarefa é representada por um par `{TaskId, Task}`, onde `TaskId` é um identificador único gerado para cada tarefa e `Task` é a própria tarefa.

A função `start/0` é utilizada para iniciar o servidor de tarefas. A função `add_task/1` adiciona uma nova tarefa ao sistema, gerando um identificador único e retornando-o como resposta. A função `get_task/1` obtém uma tarefa pelo seu ID, retornando a tarefa correspondente, caso exista. A função `complete_task/1` marca uma tarefa como concluída, removendo-a do registro de tarefas.

O registro de estado é definido como um registro de um único campo `tasks`, que é uma lista de tarefas. Cada operação no estado é realizada através de funções auxiliares, como `generate_task_id/0`, que gera um ID único para cada tarefa, `find_task/2`, que encontra uma tarefa pelo ID, e `complete_task/2`, que marca uma tarefa como concluída, removendo-a do registro de tarefas.

Espero que este exemplo atenda às suas expectativas! Se precisar de mais informações ou tiver alguma dúvida, estou à disposição.