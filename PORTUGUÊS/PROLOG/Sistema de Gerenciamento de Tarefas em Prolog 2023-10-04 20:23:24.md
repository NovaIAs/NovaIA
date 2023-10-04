Claro! Aqui está um código complexo em Prolog que cria um sistema de gerenciamento de tarefas:

```prolog
% Definição das tarefas disponíveis
tarefa(limpar_casa).
tarefa(fazer_compras).
tarefa(pagar_contas).
tarefa(preparar_jantar).
tarefa(estudar).

% Definição das dependências entre as tarefas
dependencia(limpar_casa, fazer_compras).
dependencia(fazer_compras, preparar_jantar).
dependencia(pagar_contas, estudar).

% Predicado para verificar se uma tarefa é dependente de outra
dependente(Tarefa, Dependencia) :-
    dependencia(Tarefa, Dependencia).

dependente(Tarefa, Dependencia) :-
    dependencia(Tarefa, TarefaIntermediaria),
    dependente(TarefaIntermediaria, Dependencia).

% Predicado para verificar se uma tarefa é independente
independente(Tarefa) :-
    not(dependente(_, Tarefa)).

% Predicado para retornar todas as tarefas dependentes de uma tarefa
tarefas_dependentes(Tarefa, Dependentes) :-
    findall(Dependente, dependente(Dependente, Tarefa), Dependentes).

% Predicado para retornar todas as tarefas independentes
tarefas_independentes(TarefasIndependentes) :-
    findall(Tarefa, independente(Tarefa), TarefasIndependentes).

% Predicado para imprimir todas as tarefas dependentes de uma tarefa
imprimir_dependentes(Tarefa) :-
    writeln("Tarefas dependentes da tarefa", Tarefa),
    tarefas_dependentes(Tarefa, Dependentes),
    imprimir_tarefas(Dependentes).

% Predicado para imprimir todas as tarefas independentes
imprimir_independentes :-
    writeln("Tarefas independentes:"),
    tarefas_independentes(Tarefas),
    imprimir_tarefas(Tarefas).

% Predicado auxiliar para imprimir uma lista de tarefas
imprimir_tarefas([]).
imprimir_tarefas([Tarefa | Tarefas]) :-
    writeln(Tarefa),
    imprimir_tarefas(Tarefas).
```

Este código em Prolog cria um sistema de gerenciamento de tarefas. Ele define uma lista de tarefas disponíveis e as dependências entre elas. O predicado `dependente/2` verifica se uma tarefa é dependente de outra, enquanto o predicado `independente/1` verifica se uma tarefa é independente.

O predicado `tarefas_dependentes/2` retorna todas as tarefas dependentes de uma tarefa específica e o predicado `tarefas_independentes/1` retorna todas as tarefas independentes.

Os predicados `imprimir_dependentes/1` e `imprimir_independentes/0` imprimem na tela as tarefas dependentes de uma tarefa específica e as tarefas independentes, respectivamente.

Por fim, o predicado auxiliar `imprimir_tarefas/1` é utilizado para imprimir uma lista de tarefas.

Este código pode ser utilizado como base para um sistema mais completo de gerenciamento de tarefas, permitindo adicionar, remover e atualizar tarefas, além de lidar com casos de ciclos de dependência entre as tarefas.