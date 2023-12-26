Claro! Aqui está um código em Erlang que implementa um sistema de gerenciamento de tarefas:

```erlang
-module(tarefas).
-export([iniciar/0, criar_tarefa/3, completar_tarefa/1, listar_tarefas/0]).

-includedirs("include").

-record(tarefa, {id, descricao, concluida}).

%% Inicializa o sistema de gerenciamento de tarefas
iniciar() ->
    Tarefas = ets:new(tarefas, [set, public, named_table]),
    io:format("Sistema de gerenciamento de tarefas iniciado.~n"),
    prompt().

%% Cria uma nova tarefa
criar_tarefa(ID, Descricao, Concluida) ->
    NovaTarefa = #tarefa{id = ID, descricao = Descricao, concluida = Concluida},
    ets:insert(tarefas, {ID, NovaTarefa}),
    io:format("Tarefa ~p criada com sucesso.~n", [ID]),
    prompt().

%% Marca uma tarefa como concluída
completar_tarefa(ID) ->
    Tarefa = ets:lookup(tarefas, ID),
    case Tarefa of
        [] ->
            io:format("Tarefa ~p não encontrada.~n", [ID]);
        _ ->
            ets:update_element(tarefas, ID, {3, true}),
            io:format("Tarefa ~p marcada como concluída.~n", [ID])
    end,
    prompt().

%% Lista todas as tarefas
listar_tarefas() ->
    Tarefas = ets:tab2list(tarefas),
    listar_tarefas(Tarefas).

listar_tarefas([]) ->
    io:format("Nenhuma tarefa encontrada.~n"),
    prompt();
listar_tarefas([{_, Tarefa}|Resto]) ->
    io:format("~p - ~s [~p]~n", [Tarefa#tarefa.id, Tarefa#tarefa.descricao, concluida(Tarefa#tarefa.concluida)]),
    listar_tarefas(Resto).

concluida(true) -> "Concluída";
concluida(false) -> "Pendente".

%% Exibe o prompt para o usuário
prompt() ->
    io:format("Comandos disponíveis:~n"),
    io:format("  * criar_tarefa(ID, Descricao, Concluida).~n"),
    io:format("  * completar_tarefa(ID).~n"),
    io:format("  * listar_tarefas().~n"),
    io:format("Digite um comando:~n"),
    {ok, Comando} = io:fread("", "~s"),
    case catch eval(Comando) of
        {error, Reason} ->
            io:format("Erro: ~s.~n", [Reason]),
            prompt();
        _ ->
            prompt()
    end.

%% Avalia o comando digitado pelo usuário
eval(Comando) ->
    {ok, Tokens, _} = erl_scan:string(Comando),
    {ok, [Ast]} = erl_parse:parse_exprs(Tokens),
    erl_eval:expr(Ast).
```

Este código implementa um sistema de gerenciamento de tarefas simples em Erlang. O sistema permite criar novas tarefas, marcar tarefas como concluídas e listar todas as tarefas existentes.

O código começa definindo a estrutura de dados `tarefa` usando um registro do Erlang. Em seguida, são definidas as funções exportadas `iniciar/0`, `criar_tarefa/3`, `completar_tarefa/1` e `listar_tarefas/0`.

A função `iniciar/0` inicializa o sistema de gerenciamento de tarefas, criando uma nova tabela chamada `tarefas` usando o módulo `ets` do Erlang. Em seguida, exibe uma mensagem de boas-vindas e chama a função `prompt/0`.

A função `criar_tarefa/3` recebe um ID, uma descrição e o status de conclusão da tarefa. Ela cria uma nova tarefa com base nos dados fornecidos, insere a tarefa na tabela `tarefas` e exibe uma mensagem de sucesso.

A função `completar_tarefa/1` recebe o ID de uma tarefa e marca a tarefa como concluída na tabela `tarefas`. Caso a tarefa não seja encontrada, exibe uma mensagem de erro.

A função `listar_tarefas/0` lista todas as tarefas presentes na tabela `tarefas`. Ela percorre a lista de tarefas, exibindo o ID, descrição e status de cada uma delas. Caso não haja tarefas, exibe uma mensagem informando a ausência.

A função `concluida/1` é uma função auxiliar que retorna a representação em texto do status de conclusão de uma tarefa.

Por fim, a função `prompt/0` exibe os comandos disponíveis para o usuário e aguarda a entrada de um comando. Ela chama a função `eval/1` para avaliar o comando digitado e, caso ocorra algum erro, exibe uma mensagem de erro. Após a execução do comando, chama a função `prompt/0` novamente para aguardar o próximo comando.

Esse é um exemplo de um código complexo em Erlang que implementa um sistema de gerenciamento de tarefas. Ele utiliza a biblioteca `ets` para armazenar as tarefas em uma tabela e fornece uma interface simples de linha de comando para interagir com o sistema.