Claro! Aqui está um exemplo de um código complexo em Prolog que envolve manipulação de listas e recursão:

```prolog
/* Regras */
:- op(900, xfy, '::').

% Regra para verificar se um elemento está presente em uma lista
pertence(X, [X | _]).
pertence(X, [_ | Resto]) :- pertence(X, Resto).

% Regra para concatenar duas listas
concatenar([], L, L).
concatenar([X | Resto1], L2, [X | Resto2]) :- concatenar(Resto1, L2, Resto2).

% Regra para remover um elemento de uma lista
remover(X, [X | Resto], Resto).
remover(X, [Y | Resto], [Y | NovaLista]) :- remover(X, Resto, NovaLista).

% Regra para inverter uma lista
inverter([], []).
inverter([X | Resto], ListaInvertida) :- inverter(Resto, RestoInvertido), concatenar(RestoInvertido, [X], ListaInvertida).

% Regra para encontrar o último elemento de uma lista
ultimo(X, [X]).
ultimo(X, [_ | Resto]) :- ultimo(X, Resto).

% Regra para encontrar o tamanho de uma lista
tamanho([], 0).
tamanho([_ | Resto], Tamanho) :- tamanho(Resto, TamanhoResto), Tamanho is TamanhoResto + 1.

% Regra para verificar se uma lista é palíndromo
palindromo(Lista) :- inverter(Lista, Lista), !.

% Regra para encontrar o n-ésimo elemento de uma lista
enesimo(1, [X | _], X).
enesimo(N, [_ | Resto], X) :- N > 1, N1 is N - 1, enesimo(N1, Resto, X).

% Regra para ordenar uma lista em ordem crescente
ordenar([], []).
ordenar([X | Resto], ListaOrdenada) :- particionar(X, Resto, Menor, Maior), ordenar(Menor, MenorOrdenado), ordenar(Maior, MaiorOrdenado), concatenar(MenorOrdenado, [X | MaiorOrdenado], ListaOrdenada).

% Regra auxiliar para particionar uma lista em elementos menores e maiores que um valor
particionar(_, [], [], []).
particionar(X, [Y | Resto], [Y | Menor], Maior) :- Y =< X, particionar(X, Resto, Menor, Maior).
particionar(X, [Y | Resto], Menor, [Y | Maior]) :- Y > X, particionar(X, Resto, Menor, Maior).

% Regra para encontrar a soma de todos os elementos de uma lista
soma([], 0).
soma([X | Resto], Soma) :- soma(Resto, SomaResto), Soma is X + SomaResto.

% Regra para encontrar o maior elemento de uma lista
maior([X], X).
maior([X | Resto], X) :- maior(Resto, MaiorResto), X >= MaiorResto.
maior([X | Resto], MaiorResto) :- maior(Resto, MaiorResto), X < MaiorResto.

/* Exemplos de consultas */

% Verificar se o elemento 3 está presente na lista [1, 2, 3, 4, 5]
?- pertence(3, [1, 2, 3, 4, 5]).
% Resposta: true

% Concatenar as listas [1, 2, 3] e [4, 5, 6]
?- concatenar([1, 2, 3], [4, 5, 6], ListaConcatenada).
% Resposta: ListaConcatenada = [1, 2, 3, 4, 5, 6]

% Remover o elemento 3 da lista [1, 2, 3, 4, 5]
?- remover(3, [1, 2, 3, 4, 5], ListaSem3).
% Resposta: ListaSem3 = [1, 2, 4, 5]

% Inverter a lista [1, 2, 3, 4, 5]
?- inverter([1, 2, 3, 4, 5], ListaInvertida).
% Resposta: ListaInvertida = [5, 4, 3, 2, 1]

% Encontrar o último elemento da lista [1, 2, 3, 4, 5]
?- ultimo(UltimoElemento, [1, 2, 3, 4, 5]).
% Resposta: UltimoElemento = 5

% Encontrar o tamanho da lista [1, 2, 3, 4, 5]
?- tamanho([1, 2, 3, 4, 5], TamanhoLista).
% Resposta: TamanhoLista = 5

% Verificar se a lista [1, 2, 3, 2, 1] é um palíndromo
?- palindromo([1, 2, 3, 2, 1]).
% Resposta: true

% Encontrar o 3º elemento da lista [1, 2, 3, 4, 5]
?- enesimo(3, [1, 2, 3, 4, 5], TerceiroElemento).
% Resposta: TerceiroElemento = 3

% Ordenar a lista [5, 2, 4, 1, 3]
?- ordenar([5, 2, 4, 1, 3], ListaOrdenada).
% Resposta: ListaOrdenada = [1, 2, 3, 4, 5]

% Calcular a soma dos elementos da lista [1, 2, 3, 4, 5]
?- soma([1, 2, 3, 4, 5], SomaLista).
% Resposta: SomaLista = 15

% Encontrar o maior elemento da lista [1, 10, 5, 8, 3]
?- maior([1, 10, 5, 8, 3], MaiorElemento).
% Resposta: MaiorElemento = 10
```

Esse código em Prolog contém várias regras que podem ser utilizadas para realizar diversas operações em listas, como verificar se um elemento está presente, concatenar listas, remover elementos, inverter uma lista, encontrar o último elemento, calcular o tamanho, verificar se a lista é um palíndromo, encontrar o n-ésimo elemento, ordenar uma lista, calcular a soma dos elementos e encontrar o maior elemento. Essas regras podem ser consultadas para realizar operações específicas nas listas.