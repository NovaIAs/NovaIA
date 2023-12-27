Claro! Vou gerar um código complexo em Prolog para você. Este código irá implementar um sistema de gerenciamento de tarefas, onde você poderá adicionar, remover e visualizar as tarefas.

```prolog
% Definição de fatos dinâmicos para armazenar as tarefas
:- dynamic(tarefa/2).

% Predicado principal que inicia o sistema
iniciar_sistema :-
    repeat,
    writeln(''),
    writeln('----------- SISTEMA DE GERENCIAMENTO DE TAREFAS -----------'),
    writeln('1. Adicionar uma nova tarefa'),
    writeln('2. Remover uma tarefa existente'),
    writeln('3. Visualizar todas as tarefas'),
    writeln('4. Sair do sistema'),
    writeln('-----------------------------------------------------------'),
    writeln(''),
    write('Digite a opção desejada: '),
    read(Opcao),
    realizar_acao(Opcao),
    Opcao = 4,
    !.

% Predicado para realizar a ação selecionada pelo usuário
realizar_acao(1) :-
    adicionar_tarefa.

realizar_acao(2) :-
    remover_tarefa.

realizar_acao(3) :-
    visualizar_tarefas.

realizar_acao(4) :-
    writeln('Saindo do sistema...').

realizar_acao(_) :-
    writeln('Opção inválida! Tente novamente.').

% Predicado para adicionar uma nova tarefa
adicionar_tarefa :-
    write('Digite a descrição da tarefa: '),
    read(Descricao),
    gerar_id_tarefa(Id),
    assertz(tarefa(Id, Descricao)),
    writeln('Tarefa adicionada com sucesso!').

% Predicado para remover uma tarefa existente
remover_tarefa :-
    write('Digite o ID da tarefa que deseja remover: '),
    read(Id),
    retract(tarefa(Id, _)),
    writeln('Tarefa removida com sucesso!').

% Predicado para visualizar todas as tarefas
visualizar_tarefas :-
    writeln('----------- LISTA DE TAREFAS -----------'),
    tarefa(Id, Descricao),
    writeln('ID: '),
    writeln(Id),
    writeln('Descrição: '),
    writeln(Descricao),
    fail.

visualizar_tarefas.

% Predicado para gerar um ID único para cada tarefa
gerar_id_tarefa(Id) :-
    retractall(ultimo_id_tarefa(_)),
    (   ultimo_id_tarefa(LastId) ->
        Id is LastId+1
    ;   Id is 1
    ),
    assertz(ultimo_id_tarefa(Id)).
```

Este código implementa um sistema de gerenciamento de tarefas em Prolog. Quando o sistema é iniciado, o usuário é apresentado com um menu de opções. Ele pode escolher adicionar uma nova tarefa, remover uma tarefa existente, visualizar todas as tarefas ou sair do sistema.

Ao adicionar uma nova tarefa, o usuário deve digitar a descrição dela. O sistema irá gerar um ID único para a tarefa e armazená-la na base de conhecimento.

Ao remover uma tarefa, o usuário deve digitar o ID da tarefa que deseja remover. O sistema irá remover a tarefa correspondente da base de conhecimento.

Ao visualizar todas as tarefas, o sistema irá percorrer todas as tarefas armazenadas na base de conhecimento e exibi-las na tela, mostrando o ID e a descrição de cada uma.

Este código utiliza fatos dinâmicos para armazenar as tarefas, permitindo que elas sejam adicionadas e removidas durante a execução do programa.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, por favor, me avise.