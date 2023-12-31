Claro! Aqui está um código em Erlang para gerar todos os números primos menores que um determinado valor:

```erlang
-module(primes).
-export([is_prime/1, primes_below/1]).

is_prime(N) ->
    is_prime(N, 2).

is_prime(N, D) when D*D =< N ->
    if
        N rem D == 0 ->
            false;
        true ->
            is_prime(N, D + 1)
    end;
is_prime(_, _) ->
    true.

primes_below(N) ->
    primes_below(N, N-1, []).

primes_below(_, 1, Acc) ->
    lists:reverse(Acc);
primes_below(N, X, Acc) ->
    if
        is_prime(X) ->
            primes_below(N, X-1, [X|Acc]);
        true ->
            primes_below(N, X-1, Acc)
    end.
```

Neste código, temos dois módulos exportados: `is_prime/1` e `primes_below/1`. 

- `is_prime/1` é uma função que recebe um número inteiro `N` e verifica se ele é primo. A função `is_prime` chama a função auxiliar `is_prime/2`, que possui um parâmetro adicional `D` que é o divisor a ser testado. A função `is_prime/2` verifica se `N` é divisível por `D`. Se for, retorna `false`, caso contrário, chama recursivamente a si mesma com `D+1`. Quando `D` atinge a raiz quadrada de `N`, a função retorna `true`. 

- `primes_below/1` é uma função que recebe um número inteiro `N` e retorna uma lista com todos os números primos menores que `N`. A função `primes_below` chama a função auxiliar `primes_below/3`, que possui três parâmetros: `N` (valor original informado), `X` (valor atual que está sendo verificado) e `Acc` (acumulador que armazena os números primos encontrados até o momento). A função `primes_below/3` verifica se `X` é um número primo através da função `is_prime`. Se for, adiciona `X` ao `Acc` e chama recursivamente a si mesma com `X-1`. Se não for, apenas chama recursivamente a si mesma com `X-1`. Quando `X` atinge 1, a função retorna o acumulador invertido utilizando a função `lists:reverse`.

Este código é uma implementação simples, mas eficiente, para encontrar números primos em Erlang.