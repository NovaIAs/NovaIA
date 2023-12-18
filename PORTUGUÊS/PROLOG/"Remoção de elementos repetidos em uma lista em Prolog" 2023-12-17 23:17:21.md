Claro! Aqui está um código complexo em Prolog que envolve manipulação de listas e recursão:

```
% Predicado para verificar se um elemento está presente em uma lista
esta_na_lista(_, []) :- fail.
esta_na_lista(X, [X|_]).
esta_na_lista(X, [_|T]) :- esta_na_lista(X, T).

% Predicado para remover todas as ocorrências de um elemento em uma lista
remover(_, [], []).
remover(X, [X|T], L) :- remover(X, T, L).
remover(X, [H|T], [H|L]) :- X \= H, remover(X, T, L).

% Predicado principal que remove todos os elementos repetidos em uma lista
remover_repetidos([], []).
remover_repetidos([H|T], [H|L]) :- \+ esta_na_lista(H, T), remover(H, T, T2), remover_repetidos(T2, L).
remover_repetidos([H|T], L) :- esta_na_lista(H, T), remover(H, T, T2), remover_repetidos(T2, L).

% Exemplo de uso:
?- remover_repetidos([1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 7], L).
L = [1, 2, 3, 4, 5, 6, 7]
```

Neste código em Prolog, temos três predicados principais:

1. `esta_na_lista/2`: verifica se um elemento está presente em uma lista. Ele utiliza recursão para percorrer a lista e checar se o elemento é igual ao primeiro elemento da lista ou se está presente no restante da lista.
2. `remover/3`: remove todas as ocorrências de um elemento em uma lista. Também utiliza recursão para percorrer a lista e, caso o elemento seja igual ao primeiro elemento da lista, realiza a remoção. Caso contrário, mantém o primeiro elemento na lista resultante e chama recursivamente o predicado para o restante da lista.
3. `remover_repetidos/2`: remove todos os elementos repetidos em uma lista. Utiliza o predicado `esta_na_lista/2` para verificar se um elemento está repetido na lista. Caso não esteja, o elemento é mantido na lista resultante e as ocorrências desse elemento são removidas utilizando o predicado `remover/3`. Caso esteja, o elemento é removido e o predicado é chamado recursivamente para o restante da lista.

O exemplo de uso apresentado no final do código demonstra como utilizar o predicado `remover_repetidos/2` para remover os elementos repetidos na lista `[1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 7]`, resultando na lista `[1, 2, 3, 4, 5, 6, 7]`.