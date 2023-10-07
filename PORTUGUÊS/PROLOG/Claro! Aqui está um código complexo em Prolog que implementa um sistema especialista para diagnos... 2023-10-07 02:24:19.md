Claro! Aqui está um código complexo em Prolog que implementa um sistema especialista para diagnosticar doenças com base em sintomas informados pelo usuário:

```prolog
% Definição das doenças e seus sintomas
doenca(resfriado, [febre, espirros, dor_de_garganta, congestao_nasal]).
doenca(gripe, [febre, espirros, dor_de_garganta, dor_no_corpo, fadiga]).
doenca(pneumonia, [febre, tosse, falta_de_ar, dor_no_peito]).
doenca(enxaqueca, [dor_de_cabeca, sensibilidade_a_luz, sensibilidade_a_sons]).
doenca(gastrite, [dor_de_estomago, azia, perda_de_apetite]).

% Regras para o diagnóstico
diagnostico(Sintomas, Doenca) :-
    doenca(Doenca, ListaSintomas),
    verificar_sintomas(Sintomas, ListaSintomas).

verificar_sintomas([], _).
verificar_sintomas([Sintoma|RestoSintomas], ListaSintomas) :-
    member(Sintoma, ListaSintomas),
    verificar_sintomas(RestoSintomas, ListaSintomas).

% Consulta do usuário
:- initialization(main).
main :-
    writeln("Bem-vindo(a) ao sistema especialista de diagnóstico médico!"),
    writeln("Por favor, informe seus sintomas separados por vírgula:"),
    read_line_to_codes(user_input, Input),
    atom_codes(AtomInput, Input),
    atomic_list_concat(ListaSintomas, ',', AtomInput),
    diagnostico(ListaSintomas, Doenca),
    format("Baseado nos sintomas informados, você pode ter ~w.", [Doenca]).
```

Explicação:

Este código em Prolog implementa um sistema especialista de diagnóstico médico. Ele possui uma base de conhecimento com doenças e seus sintomas associados. O usuário informa seus sintomas separados por vírgula e o sistema verifica quais doenças podem estar relacionadas a esses sintomas.

Primeiro, definimos as doenças e seus sintomas usando o predicado `doenca/2`. Cada doença é representada por um átomo e seus sintomas são armazenados em uma lista.

Em seguida, temos a regra `diagnostico/2`, que recebe a lista de sintomas informados pelo usuário e a doença correspondente é encontrada usando a regra `verificar_sintomas/2`.

A regra `verificar_sintomas/2` verifica se cada sintoma informado pelo usuário está presente na lista de sintomas da doença. Isso é feito usando o predicado `member/2`, que verifica se um elemento pertence a uma lista.

Por fim, no bloco `main`, o programa é iniciado. O usuário é solicitado a informar seus sintomas e esses sintomas são lidos a partir do input do usuário. Em seguida, os sintomas são convertidos em uma lista de átomos usando a função `read_line_to_codes/2` e `atom_codes/2`.

O predicado `diagnostico/2` é chamado com a lista de sintomas informados pelo usuário e a doença correspondente é encontrada. O resultado é exibido para o usuário usando o predicado `format/2`.

Este sistema especialista em Prolog é capaz de diagnosticar doenças com base nos sintomas informados pelo usuário, fornecendo uma resposta com base nas regras definidas na base de conhecimento.