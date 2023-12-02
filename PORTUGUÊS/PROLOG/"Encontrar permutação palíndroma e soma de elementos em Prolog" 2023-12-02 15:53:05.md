Claro! Aqui está um código complexo em Prolog que envolve manipulação de listas e recursão:

```prolog
% Predicado para verificar se um elemento está presente em uma lista
esta_na_lista(_, []) :- fail.
esta_na_lista(X, [X|_]).
esta_na_lista(X, [_|T]) :- esta_na_lista(X, T).

% Predicado para concatenar duas listas
concatena([], L, L).
concatena([H|T], L, [H|Resultado]) :- concatena(T, L, Resultado).

% Predicado para inverter uma lista
inverte([], []).
inverte([H|T], Resultado) :- inverte(T, Invertido), concatena(Invertido, [H], Resultado).

% Predicado para remover um elemento de uma lista
remove(_, [], []).
remove(X, [X|T], T).
remove(X, [H|T], [H|Resultado]) :- remove(X, T, Resultado).

% Predicado para encontrar todas as permutações de uma lista
permutacoes([], []).
permutacoes(Lista, [Elemento|PermutacoesRestantes]) :- 
    esta_na_lista(Elemento, Lista), 
    remove(Elemento, Lista, ListaRestante), 
    permutacoes(ListaRestante, PermutacoesRestantes).

% Predicado para verificar se uma lista é palíndromo
palindromo(Lista) :- inverte(Lista, Lista), !.

% Predicado para encontrar a soma de todos os elementos de uma lista
soma_lista([], 0).
soma_lista([H|T], Soma) :- soma_lista(T, SomaRestante), Soma is SomaRestante + H.

% Predicado principal que combina todos os predicados anteriores
main(Lista) :-
    permutacoes(Lista, Permutacoes),
    member(Permutacao, Permutacoes),
    palindromo(Permutacao),
    soma_lista(Permutacao, Soma),
    write('Permutação palíndroma encontrada: '), write(Permutacao), nl,
    write('Soma dos elementos: '), write(Soma).
```

Neste código, temos uma série de predicados em Prolog que realizam diferentes operações em listas. O predicado `esta_na_lista/2` verifica se um elemento está presente em uma lista. O predicado `concatena/3` concatena duas listas. O predicado `inverte/2` inverte uma lista. O predicado `remove/3` remove um elemento de uma lista. O predicado `permutacoes/2` gera todas as permutações de uma lista. O predicado `palindromo/1` verifica se uma lista é um palíndromo. O predicado `soma_lista/2` calcula a soma de todos os elementos de uma lista.

Por fim, o predicado `main/1` combina todos os predicados anteriores para encontrar uma permutação palíndroma da lista dada e exibir a permutação e a soma dos elementos.

Espero que este código atenda às suas expectativas! Lembre-se de que você pode adaptá-lo e modificá-lo conforme necessário.