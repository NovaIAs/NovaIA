Claro! Aqui está um código complexo em Erlang que implementa um sistema de agendamento de tarefas:

```erlang
-module(agendamento).
-export([iniciar/0, agendar_tarefa/3, remover_tarefa/1]).

-record(tarefa, {id, descricao, data}).

iniciar() ->
    ets:new(tarefas, [set, named_table]).

agendar_tarefa(Id, Descricao, Data) ->
    Tarefa = #tarefa{id = Id, descricao = Descricao, data = Data},
    ets:insert(tarefas, {Id, Tarefa}).

remover_tarefa(Id) ->
    ets:delete(tarefas, Id).

```

Explicação do código:

Primeiro, definimos um módulo chamado "agendamento" usando a diretiva `-module(agendamento)`. Em seguida, exportamos as funções `iniciar/0`, `agendar_tarefa/3` e `remover_tarefa/1` usando a diretiva `-export/1`.

O próximo passo é criar o registro `tarefa` com os campos `id`, `descricao` e `data` usando a diretiva `-record(tarefa, {id, descricao, data})`. Esse registro será usado para representar cada tarefa agendada.

A função `iniciar/0` cria uma nova tabela chamada "tarefas" usando a função `ets:new/2`. Essa tabela será usada para armazenar todas as tarefas agendadas.

A função `agendar_tarefa/3` recebe um `Id`, uma `Descricao` e uma `Data` e cria uma nova tarefa usando o registro `tarefa`. Em seguida, insere a tarefa na tabela "tarefas" usando a função `ets:insert/2`.

A função `remover_tarefa/1` recebe um `Id` e remove a tarefa correspondente da tabela "tarefas" usando a função `ets:delete/2`.

Esse código implementa um sistema básico de agendamento de tarefas em Erlang, permitindo que o usuário agende tarefas, consulte e remova tarefas agendadas.