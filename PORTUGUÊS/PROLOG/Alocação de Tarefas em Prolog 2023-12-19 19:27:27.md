Claro! Aqui está um exemplo de código em Prolog que envolve um problema de alocação de tarefas em um conjunto de pessoas:

```prolog
% Definindo as pessoas
pessoa(joao).
pessoa(maria).
pessoa(pedro).
pessoa(ana).

% Definindo as tarefas
tarefa(limpar_casa).
tarefa(cozinhar).
tarefa(lavar_roupa).
tarefa(ir_as_compras).

% Definindo a relação entre pessoas e tarefas
realiza(joao, limpar_casa).
realiza(maria, cozinhar).
realiza(pedro, lavar_roupa).
realiza(ana, ir_as_compras).

% Definindo as restrições
% Cada pessoa deve realizar apenas uma tarefa
:- use_module(library(clpfd)).

alocacao_tarefas(Pessoas, Tarefas) :-
    length(Pessoas, NumPessoas),
    length(Tarefas, NumTarefas),
    length(Alocacao, NumPessoas),
    domain(Alocacao, 1, NumTarefas),

    % Restrição de unidade: cada pessoa realiza apenas uma tarefa
    all_distinct(Alocacao),

    % Restrição de que todas as tarefas sejam realizadas
    global_cardinality(Alocacao, Tarefas),

    % Restrição de que cada tarefa seja realizada por apenas uma pessoa
    inverse(Alocacao, Inverse),
    all_distinct(Inverse),

    % Pesquisar a solução
    labeling([], Alocacao),

    % Imprimir a solução
    imprime_alocacao(Pessoas, Tarefas, Alocacao).

% Função auxiliar para imprimir a alocação de tarefas
imprime_alocacao([], [], []).
imprime_alocacao([Pessoa | Pessoas], [Tarefa | Tarefas], [Alocacao | Alocacoes]) :-
    format("Pessoa ~w realiza a tarefa de ~w~n", [Pessoa, Tarefa]),
    imprime_alocacao(Pessoas, Tarefas, Alocacoes).
```

Neste código, temos um conjunto de pessoas (joao, maria, pedro, ana) e um conjunto de tarefas (limpar_casa, cozinhar, lavar_roupa, ir_as_compras). A relação entre as pessoas e as tarefas é definida através do predicado `realiza/2`.

A função `alocacao_tarefas/2` é responsável por encontrar uma alocação de tarefas para as pessoas, respeitando as restrições impostas. A restrição de que cada pessoa deve realizar apenas uma tarefa é garantida pela restrição `all_distinct/1`. A restrição de que todas as tarefas sejam realizadas é garantida pela restrição `global_cardinality/2`. A restrição de que cada tarefa seja realizada por apenas uma pessoa é garantida pela restrição `inverse/2`.

No final, a função `imprime_alocacao/3` é responsável por imprimir a alocação de tarefas encontrada.