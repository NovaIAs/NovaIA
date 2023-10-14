Claro! Aqui está um exemplo de código em Prolog que utiliza recursão para percorrer uma lista e verificar se todos os elementos são números primos:

```prolog
% Predicado principal que verifica se todos os elementos de uma lista são números primos
todos_primos([]).
todos_primos([H|T]) :-
    primo(H),
    todos_primos(T).

% Predicado auxiliar que verifica se um número é primo
primo(2).
primo(N) :-
    N > 2,
    primo_aux(N, 2).

% Predicado auxiliar recursivo que verifica se um número é divisível por algum número menor que ele
primo_aux(N, Div) :-
    Div < N,
    (N mod Div) =\= 0,
    NextDiv is Div + 1,
    primo_aux(N, NextDiv).
primo_aux(N, Div) :-
    Div >= N.

% Exemplo de uso
?- todos_primos([2, 3, 5, 7]).
```

Neste código, temos um predicado principal chamado `todos_primos/1` que recebe uma lista como parâmetro. Ele é responsável por percorrer recursivamente a lista e verificar se todos os elementos são números primos.

O predicado `todos_primos/1` possui duas cláusulas. A primeira cláusula é a base da recursão e define que uma lista vazia já possui todos os elementos como primos. A segunda cláusula é a recursiva, ela verifica se o primeiro elemento da lista é primo utilizando o predicado `primo/1` e, em seguida, chama recursivamente o `todos_primos/1` passando o restante da lista.

O predicado `primo/1` é responsável por verificar se um número é primo ou não. Ele possui duas cláusulas. A primeira cláusula define que o número 2 é primo. A segunda cláusula é a recursiva e verifica se o número é maior que 2 e não é divisível por nenhum número menor que ele utilizando o predicado auxiliar `primo_aux/2`.

O predicado `primo_aux/2` é responsável por verificar se um número é divisível por algum número menor que ele. Ele possui duas cláusulas. A primeira cláusula é a recursiva e verifica se o número não é divisível pelo valor atual de `Div`. Se for o caso, incrementa o valor de `Div` e chama recursivamente o `primo_aux/2`. A segunda cláusula é a base da recursão e verifica se o valor atual de `Div` é maior ou igual ao número, ou seja, verifica se o número não é divisível por nenhum número menor que ele.

No exemplo de uso, a consulta `todos_primos([2, 3, 5, 7])` retorna `true`, indicando que todos os elementos da lista são números primos.