Claro! Aqui está um exemplo de um código complexo em Erlang que implementa um sistema de gerenciamento de tarefas:

```erlang
-module(task_manager).
-export([start/0, add_task/1, complete_task/1, list_tasks/0]).

start() ->
    io:format("Bem-vindo ao Gerenciador de Tarefas!~n"),
    init_state().

init_state() ->
    {[], 0}.

add_task(Task) ->
    {Tasks, TaskId} = get_state(),
    NewTask = {TaskId, Task},
    NewTasks = [NewTask | Tasks],
    NewTaskId = TaskId + 1,
    update_state(NewTasks, NewTaskId),
    io:format("Tarefa adicionada com sucesso. ID: ~p~n", [TaskId]).

complete_task(TaskId) ->
    {Tasks, _} = get_state(),
    case lists:keyfind(TaskId, 1, Tasks) of
        false -> io:format("Tarefa não encontrada.~n");
        {_, Task} ->
            NewTasks = lists:keydelete(TaskId, 1, Tasks),
            update_state(NewTasks, TaskId),
            io:format("Tarefa concluída: ~s~n", [Task])
    end.

list_tasks() ->
    {Tasks, _} = get_state(),
    io:format("Tarefas pendentes:~n"),
    print_tasks(Tasks).

print_tasks([]) ->
    io:format("Nenhuma tarefa pendente.~n");
print_tasks([{TaskId, Task} | Rest]) ->
    io:format("ID: ~p, Tarefa: ~s~n", [TaskId, Task]),
    print_tasks(Rest).

get_state() ->
    {Tasks, TaskId} = process_flag(registers, {_, _}),
    {Tasks, TaskId}.

update_state(Tasks, TaskId) ->
    process_flag(registers, {Tasks, TaskId}).
```

Neste código, criamos um módulo chamado `task_manager` que contém quatro funções exportadas: `start/0`, `add_task/1`, `complete_task/1` e `list_tasks/0`.

A função `start/0` é responsável por imprimir uma mensagem de boas-vindas e chamar a função `init_state/0`. A função `init_state/0` inicializa o estado do gerenciador de tarefas como uma lista vazia de tarefas e um ID de tarefa igual a 0.

A função `add_task/1` recebe uma tarefa como parâmetro e adiciona essa tarefa ao estado do gerenciador de tarefas. A função gera um novo ID de tarefa, atualiza o estado e imprime o ID da tarefa adicionada.

A função `complete_task/1` recebe um ID de tarefa como parâmetro e marca a tarefa correspondente como concluída. Se a tarefa não for encontrada, uma mensagem de erro é impressa. Caso contrário, a tarefa é removida do estado e uma mensagem de confirmação é impressa.

A função `list_tasks/0` imprime todas as tarefas pendentes no estado do gerenciador de tarefas. Se não houver tarefas pendentes, uma mensagem informando isso é impressa.

Essas são apenas algumas funcionalidades básicas de um sistema de gerenciamento de tarefas em Erlang. É possível adicionar mais funcionalidades, como editar tarefas, definir prazos, atribuir responsáveis, entre outras.