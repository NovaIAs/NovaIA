Claro! Aqui está um exemplo de um código complexo em Erlang. Este programa é um exemplo de um sistema de gerenciamento de tarefas em uma equipe de desenvolvimento de software.

```erlang
-module(gerenciamento_tarefas).
-export([
    adicionar_tarefa/4,
    remover_tarefa/2,
    atribuir_tarefa/3,
    concluir_tarefa/2,
    listar_tarefas/1
]).

-record(tarefa, {id, descricao, responsavel, concluida}).

%% Função para adicionar uma tarefa ao sistema
adicionar_tarefa(Id, Descricao, Responsavel, Concluida) ->
    NovaTarefa = #tarefa{id = Id, descricao = Descricao, responsavel = Responsavel, concluida = Concluida},
    SalvarTarefa = salva_tarefa(NovaTarefa),
    case SalvarTarefa of
        {ok, _} ->
            io:format("Tarefa adicionada com sucesso.~n");
        {error, Reason} ->
            io:format("Erro ao adicionar tarefa: ~p~n", [Reason])
    end.

%% Função para remover uma tarefa do sistema
remover_tarefa(Id, Responsavel) ->
    case busca_tarefa(Id) of
        #tarefa{responsavel = Responsavel} ->
            remover_tarefa(Id),
            io:format("Tarefa removida com sucesso.~n");
        _ ->
            io:format("Você não tem permissão para remover essa tarefa.~n")
    end.

%% Função para atribuir uma tarefa a um responsável
atribuir_tarefa(Id, Responsavel, NovoResponsavel) ->
    case busca_tarefa(Id) of
        #tarefa{responsavel = Responsavel} ->
            AtualizarTarefa = atualiza_responsavel(Id, NovoResponsavel),
            case AtualizarTarefa of
                {ok, _} ->
                    io:format("Responsável pela tarefa atualizado com sucesso.~n");
                {error, Reason} ->
                    io:format("Erro ao atualizar responsável pela tarefa: ~p~n", [Reason])
            end;
        _ ->
            io:format("Você não tem permissão para atribuir essa tarefa.~n")
    end.

%% Função para concluir uma tarefa
concluir_tarefa(Id, Responsavel) ->
    case busca_tarefa(Id) of
        #tarefa{responsavel = Responsavel} ->
            AtualizarTarefa = atualiza_concluida(Id, true),
            case AtualizarTarefa of
                {ok, _} ->
                    io:format("Tarefa concluída com sucesso.~n");
                {error, Reason} ->
                    io:format("Erro ao concluir tarefa: ~p~n", [Reason])
            end;
        _ ->
            io:format("Você não tem permissão para concluir essa tarefa.~n")
    end.

%% Função para listar todas as tarefas de um responsável
listar_tarefas(Responsavel) ->
    Tarefas = listar_tarefas_responsavel(Responsavel),
    io:format("Tarefas do responsável ~p:~n", [Responsavel]),
    listar_tarefas_aux(Tarefas).

%% Funções auxiliares

salva_tarefa(Tarefa) ->
    %% Lógica para salvar a tarefa no sistema
    {ok, Tarefa}.

remover_tarefa(Id) ->
    %% Lógica para remover a tarefa do sistema
    ok.

busca_tarefa(Id) ->
    %% Lógica para buscar a tarefa no sistema
    %% Retorna a tarefa encontrada ou undefined se não encontrada
    #tarefa{id = Id}.

atualiza_responsavel(Id, NovoResponsavel) ->
    %% Lógica para atualizar o responsável da tarefa
    {ok, Tarefa}.

atualiza_concluida(Id, Concluida) ->
    %% Lógica para atualizar o status da tarefa (concluída ou não)
    {ok, Tarefa}.

listar_tarefas_responsavel(Responsavel) ->
    %% Lógica para buscar todas as tarefas de um responsável
    %% Retorna uma lista de tarefas
    [].

listar_tarefas_aux([]) ->
    ok;
listar_tarefas_aux([Tarefa | Resto]) ->
    io:format("~p~n", [Tarefa]),
    listar_tarefas_aux(Resto).
```

Neste código, definimos um módulo `gerenciamento_tarefas` com várias funções para adicionar, remover, atribuir, concluir e listar tarefas. 

Cada tarefa é representada por um registro `tarefa` com os campos `id`, `descricao`, `responsavel` e `concluida`. Os registros são utilizados para facilitar o acesso aos dados de uma tarefa.

As funções `adicionar_tarefa/4`, `remover_tarefa/2`, `atribuir_tarefa/3` e `concluir_tarefa/2` têm lógicas diferentes dependendo da permissão do usuário. Se o usuário não tiver permissão para realizar a ação, uma mensagem de erro é exibida.

A função `listar_tarefas/1` recebe o nome de um responsável e lista todas as tarefas atribuídas a ele.

As funções auxiliares `salva_tarefa/1`, `remover_tarefa/1`, `busca_tarefa/1`, `atualiza_responsavel/2`, `atualiza_concluida/2`, `listar_tarefas_responsavel/1` são apenas placeholders e precisariam ser implementadas adequadamente no contexto do sistema.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, estou aqui para ajudar.