Claro! Aqui está um código complexo em Prolog que utiliza recursão para calcular o fatorial de um número e possui um predicado adicional para verificar se um número é um número perfeito:

```
% Predicado para calcular o fatorial de um número
fatorial(0, 1). % Caso base: fatorial de 0 é 1
fatorial(N, F) :-
    N > 0,
    N1 is N - 1,
    fatorial(N1, F1),
    F is N * F1.

% Predicado para verificar se um número é perfeito
perfeito(N) :-
    soma_divisores(N, 1, S),
    N =:= S.

% Predicado auxiliar para calcular a soma dos divisores de um número
soma_divisores(N, N, 0).
soma_divisores(N, D, S) :-
    D < N,
    N mod D =:= 0,
    D1 is D + 1,
    soma_divisores(N, D1, S1),
    S is S1 + D.
soma_divisores(N, D, S) :-
    D < N,
    N mod D =\= 0,
    D1 is D + 1,
    soma_divisores(N, D1, S).

% Exemplos de uso:
?- fatorial(5, Fatorial).
Fatorial = 120.

?- perfeito(6).
true.

?- perfeito(28).
true.

?- perfeito(12).
false.
```

Neste código, o predicado `fatorial/2` calcula o fatorial de um número utilizando recursão. Ele possui um caso base onde o fatorial de 0 é definido como 1. Em seguida, utiliza a recursão para calcular o fatorial de um número `N` multiplicando-o pelo fatorial do número `N-1`.

O predicado `perfeito/1` verifica se um número é perfeito. Um número é considerado perfeito se a soma de seus divisores (exceto ele mesmo) é igual a ele mesmo. Para calcular a soma dos divisores, utilizamos o predicado auxiliar `soma_divisores/3` que utiliza recursão para somar os divisores de um número até alcançar o próprio número.

Os exemplos de uso demonstram o cálculo do fatorial de 5, bem como a verificação dos números 6, 28 e 12 para saber se são números perfeitos.