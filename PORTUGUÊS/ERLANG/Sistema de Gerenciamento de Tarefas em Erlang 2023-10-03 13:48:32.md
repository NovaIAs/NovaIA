Claro! Aqui está um exemplo de código em Erlang que implementa um sistema de gerenciamento de tarefas:

```erlang
-module(gerenciador_tarefas).
-export([iniciar/0, adicionar_tarefa/2, remover_tarefa/1, listar_tarefas/0]).

% Representa uma tarefa com um identificador e uma descrição
-record(tarefa, {id, descricao}).

% Inicia o gerenciador de tarefas com uma lista vazia
iniciar() ->
    {gerenciador_tarefas, ets:new(tarefas, [set, named_table])}.

% Adiciona uma nova tarefa ao gerenciador
adicionar_tarefa(Gerenciador, Descricao) ->
    Tarefa = #tarefa{id = erlang:unique_integer([positive]), descricao = Descricao},
    ets:insert(Gerenciador, Tarefa),
    io:format("Tarefa adicionada com sucesso. ID: ~p~n", [Tarefa#tarefa.id]).

% Remove uma tarefa existente do gerenciador
remover_tarefa(Gerenciador, Id) ->
    case ets:lookup_element(Gerenciador, Id, 2) of
        {Id, Descricao} ->
            ets:delete(Gerenciador, Id),
            io:format("Tarefa removida com sucesso. ID: ~p, Descrição: ~p~n", [Id, Descricao]);
        false ->
            io:format("Tarefa não encontrada. ID: ~p~n", [Id])
    end.

% Lista todas as tarefas presentes no gerenciador
listar_tarefas(Gerenciador) ->
    Tarefas = ets:tab2list(Gerenciador),
    io:format("Tarefas no gerenciador:~n"),
    listar_tarefas_aux(Tarefas).

% Função auxiliar para listar as tarefas
listar_tarefas_aux([]) ->
    io:format("Nenhuma tarefa encontrada.~n");
listar_tarefas_aux([Tarefa | Resto]) ->
    io:format("ID: ~p, Descrição: ~p~n", [Tarefa#tarefa.id, Tarefa#tarefa.descricao]),
    listar_tarefas_aux(Resto).
```

Este código implementa um gerenciador de tarefas simples usando a biblioteca ETS (Erlang Term Storage). 

A função `iniciar/0` cria uma nova tabela ETS chamada "tarefas" para armazenar as tarefas.

A função `adicionar_tarefa/2` recebe uma descrição de tarefa como parâmetro e cria uma nova tarefa com um identificador único gerado usando `erlang:unique_integer/1`. A tarefa é então inserida na tabela ETS e uma mensagem é exibida informando o ID da tarefa adicionada.

A função `remover_tarefa/2` recebe um ID de tarefa como parâmetro e verifica se a tarefa existe na tabela ETS. Se a tarefa existir, ela é removida da tabela e uma mensagem é exibida informando o ID da tarefa removida e sua descrição. Caso contrário, uma mensagem é exibida informando que a tarefa não foi encontrada.

A função `listar_tarefas/1` lista todas as tarefas presentes na tabela ETS. Primeiro, as tarefas são convertidas em uma lista usando `ets:tab2list/1`. Em seguida, a função auxiliar `listar_tarefas_aux/1` é chamada para exibir o ID e a descrição de cada tarefa. Se não houver nenhuma tarefa na lista, uma mensagem informando que nenhuma tarefa foi encontrada é exibida.

Espero que este exemplo atenda às suas expectativas!