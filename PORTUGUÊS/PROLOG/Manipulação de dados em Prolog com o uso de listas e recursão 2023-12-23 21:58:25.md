Claro! Aqui está um código complexo em Prolog que envolve o uso de listas e recursão para realizar uma série de manipulações de dados:

```prolog
% Predicado para verificar se um elemento pertence a uma lista
pertence(X, [X|_]).
pertence(X, [_|T]) :-
    pertence(X, T).

% Predicado para remover a primeira ocorrência de um elemento em uma lista
remover(_, [], []).
remover(X, [X|T], T).
remover(X, [H|T], [H|Resto]) :-
    remover(X, T, Resto).

% Predicado para inverter uma lista
inverter([], []).
inverter([H|T], Invertida) :-
    inverter(T, Resto),
    append(Resto, [H], Invertida).

% Predicado para concatenar duas listas
append([], L, L).
append([H|T], L, [H|Resultado]) :-
    append(T, L, Resultado).

% Predicado para encontrar o último elemento de uma lista
ultimo([X], X).
ultimo([_|T], Ultimo) :-
    ultimo(T, Ultimo).

% Predicado para remover o último elemento de uma lista
remover_ultimo([_], []).
remover_ultimo([H|T], [H|Resto]) :-
    remover_ultimo(T, Resto).

% Predicado para dividir uma lista em duas partes iguais
dividir(Lista, Parte1, Parte2) :-
    length(Lista, Tamanho),
    Meio is Tamanho // 2,
    length(Parte1, Meio),
    length(Parte2, Meio),
    append(Parte1, Parte2, Lista).

% Predicado para verificar se uma lista está ordenada em ordem crescente
ordenada([]).
ordenada([_]).
ordenada([X,Y|T]) :-
    X =< Y,
    ordenada([Y|T]).

% Predicado para ordenar uma lista usando o algoritmo bubble sort
bubble_sort(Lista, Ordenada) :-
    ordenada(Lista),
    Ordenada = Lista.
bubble_sort(Lista, Ordenada) :-
    trocar(Lista, Trocada),
    bubble_sort(Trocada, Ordenada).

% Predicado auxiliar para trocar dois elementos consecutivos em uma lista
trocar([X,Y|T], [Y,X|T]) :-
    X > Y.
trocar([Z|T1], [Z|T2]) :-
    trocar(T1, T2).

% Predicado principal para realizar uma série de manipulações de dados
realizar_manipulacoes(Lista, Resultado) :-
    pertence(5, Lista),
    remover(5, Lista, ListaSem5),
    inverter(ListaSem5, Invertida),
    ultimo(Invertida, Ultimo),
    remover_ultimo(Invertida, InvertidaSemUltimo),
    dividir(InvertidaSemUltimo, Parte1, Parte2),
    bubble_sort(Parte1, Parte1Ordenada),
    bubble_sort(Parte2, Parte2Ordenada),
    append(Parte1Ordenada, Parte2Ordenada, Resultado).
```

Neste código, são definidos uma série de predicados para realizar diferentes manipulações em uma lista. O predicado `realizar_manipulacoes/2` é o principal e realiza as seguintes ações:

1. Verifica se o número 5 pertence à lista.
2. Remove a primeira ocorrência do número 5 na lista.
3. Inverte a lista resultante da etapa anterior.
4. Encontra o último elemento da lista invertida.
5. Remove o último elemento da lista invertida.
6. Divide a lista invertida sem o último elemento em duas partes iguais.
7. Ordena cada parte da lista usando o algoritmo bubble sort.
8. Concatena as duas partes ordenadas para obter o resultado final.

Espero que este exemplo atenda às suas expectativas!