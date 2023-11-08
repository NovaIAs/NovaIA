Claro, vou criar um código em Erlang que envolve a implementação de um sistema de gerenciamento de tarefas com concorrência e persistência em um banco de dados SQLite. Lembre-se de que este é um exemplo complexo e pode não ser o mais eficiente ou o mais prático para todas as situações.

```erlang
-module(task_manager).
-compile([export_all]).

% Definição do tipo de dados para representar uma tarefa
-record(task, {id, description, completed}).

% Função para iniciar o banco de dados (SQLite)
start_database() ->
    sqlite:start(),
    sqlite:exec("CREATE TABLE tasks (id INTEGER PRIMARY KEY, description TEXT, completed BOOLEAN);").

% Função para adicionar uma nova tarefa ao banco de dados
add_task(Description) ->
    sqlite:exec("INSERT INTO tasks (description, completed) VALUES (?, ?);", [Description, false]),
    {ok, #task{id=sqlite:last_insert_rowid(), description=Description, completed=false}}.

% Função para buscar todas as tarefas no banco de dados
get_all_tasks() ->
    {ok, Rows} = sqlite:fetch("SELECT * FROM tasks;"),
    lists:map(fun([Id, Desc, Completed]) -> #task{id=Id, description=Desc, completed=Completed /= 0} end, Rows).

% Função para buscar uma tarefa pelo seu ID
get_task_by_id(Id) ->
    {ok, [[Id, Desc, Completed]]} = sqlite:fetch("SELECT * FROM tasks WHERE id = ?;", [Id]),
    #task{id=Id, description=Desc, completed=Completed /= 0}.

% Função para marcar uma tarefa como concluída
complete_task(Id) ->
    sqlite:exec("UPDATE tasks SET completed = ? WHERE id = ?;", [true, Id]),
    get_task_by_id(Id).

% Função para deletar uma tarefa
delete_task(Id) ->
    sqlite:exec("DELETE FROM tasks WHERE id = ?;", [Id]).

% Função para parar o banco de dados
stop_database() ->
    sqlite:stop().
```

Explicação do código:
- `task_manager` é o nome do módulo.
- `record(task, {id, description, completed}).` define uma estrutura de dados chamada `task` com três campos: `id`, `description` e `completed`.
- `start_database()` inicia o banco de dados SQLite e cria uma tabela chamada `tasks` para armazenar as tarefas.
- `add_task(Description)` adiciona uma nova tarefa ao banco de dados com a descrição fornecida e retorna um registro da tarefa criada.
- `get_all_tasks()` retorna uma lista de todas as tarefas no banco de dados.
- `get_task_by_id(Id)` retorna a tarefa com o ID especificado.
- `complete_task(Id)` marca uma tarefa como concluída.
- `delete_task(Id)` exclui uma tarefa do banco de dados.
- `stop_database()` para o banco de dados SQLite.

Esse código cria um sistema de gerenciamento de tarefas simples, utilizando um banco de dados SQLite para persistência. Ele permite adicionar, listar, marcar como concluída e excluir tarefas. Por favor, note que este é apenas um exemplo e pode ser expandido e otimizado de acordo com as necessidades específicas do projeto.