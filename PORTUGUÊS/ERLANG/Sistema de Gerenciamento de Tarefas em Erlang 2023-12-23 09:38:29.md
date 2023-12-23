Claro! Aqui está um exemplo de um código em Erlang que implementa um sistema distribuído de gerenciamento de tarefas em um ambiente de produção.

```erlang
-module(gerenciador_tarefas).
-export([iniciar/0, adicionar_tarefa/1, executar_tarefas/0]).

% Tarefas é uma lista que irá armazenar as tarefas a serem executadas
% em formato de tuplas {Tarefa, Status}
% O Status pode ser "pendente", "executando" ou "concluído"
% Para fins de simplicidade, as tarefas são representadas por números inteiros
% Neste exemplo, utilizaremos apenas um processo para gerenciar as tarefas
% Em um ambiente real, poderia haver vários processos distribuídos em diferentes nós

% Função para iniciar o gerenciador de tarefas
iniciar() ->
    spawn(fun() -> loop([]) end).

% Função auxiliar para adicionar uma tarefa à lista de tarefas
adicionar_tarefa(Tarefa) ->
    gen_server:call(?MODULE, {adicionar, Tarefa}).

% Função para executar as tarefas pendentes
executar_tarefas() ->
    gen_server:call(?MODULE, executar).

% Função principal que implementa o loop do gerenciador de tarefas
loop(Tarefas) ->
    receive
        {adicionar, Tarefa} ->
            NovaLista = [{Tarefa, pendente} | Tarefas],
            loop(NovaLista);
        executar ->
            TarefasEmExecucao = lists:filter(fun({_Tarefa, Status}) -> Status == pendente end, Tarefas),
            NovaLista = executar_tarefas(TarefasEmExecucao, Tarefas),
            loop(NovaLista)
    end.

% Função auxiliar para executar as tarefas pendentes
executar_tarefas([], Tarefas) ->
    Tarefas;
executar_tarefas([{Tarefa, pendente} | Resto], Tarefas) ->
    io:format("Executando tarefa ~w~n", [Tarefa]),
    NovaTarefa = {Tarefa, executando},
    NovaLista = lists:keyreplace(Tarefa, 1, Tarefas, NovaTarefa),
    timer:sleep(1000),
    io:format("Tarefa ~w concluída~n", [Tarefa]),
    NovaTarefa2 = {Tarefa, concluido},
    NovaLista2 = lists:keyreplace(Tarefa, 1, NovaLista, NovaTarefa2),
    executar_tarefas(Resto, NovaLista2).

% Utilizando a biblioteca gen_server para fornecer uma interface mais amigável
% para o gerenciador de tarefas

% Callbacks gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Função de inicialização do gen_server
init([]) ->
    {ok, []}.

% Função para lidar com chamadas síncronas ao gen_server
handle_call({adicionar, Tarefa}, _From, Tarefas) ->
    NovaLista = [{Tarefa, pendente} | Tarefas],
    {reply, ok, NovaLista};
handle_call(executar, _From, Tarefas) ->
    {reply, ok, Tarefas}.

% Função para lidar com mensagens assíncronas ao gen_server
handle_cast(_Msg, Tarefas) ->
    {noreply, Tarefas}.

% Função para lidar com informações não tratadas pelo gen_server
handle_info(_Info, Tarefas) ->
    {noreply, Tarefas}.

% Função chamada quando o gen_server é encerrado
terminate(_Reason, _Tarefas) ->
    ok.

% Função chamada quando ocorre uma alteração de código
code_change(_OldVsn, Tarefas, _Extra) ->
    {ok, Tarefas}.
```

Este código implementa um sistema de gerenciamento de tarefas onde as tarefas são adicionadas a uma lista de tarefas e são executadas em um processo separado. Quando uma tarefa é adicionada, ela é marcada como "pendente" na lista de tarefas. Ao chamar a função `executar_tarefas`, as tarefas pendentes são percorridas e executadas uma a uma. Durante a execução de cada tarefa, ela é marcada como "executando" e uma mensagem é exibida. Em seguida, a tarefa "dorme" por 1 segundo simulando o processamento. Após a conclusão da tarefa, ela é marcada como "concluída" e uma mensagem é exibida. O código utiliza a biblioteca `gen_server` para fornecer uma interface mais amigável para o gerenciador de tarefas.

Este código é apenas um exemplo e pode ser aprimorado e adaptado de acordo com os requisitos específicos do sistema de gerenciamento de tarefas.