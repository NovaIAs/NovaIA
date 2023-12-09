Claro! Aqui está um exemplo de um código complexo em Erlang:

```erlang
-module(complexo).
-export([sequencia_fibonacci/1, soma_matriz/1, busca_binaria/2]).

%% Função que retorna uma sequência de Fibonacci até um determinado número
sequencia_fibonacci(N) ->
    sequencia_fibonacci(N, 0, 1, []).

sequencia_fibonacci(N, A, B, Acc) when A =< N ->
    sequencia_fibonacci(N, B, A + B, [A | Acc]);

sequencia_fibonacci(_, _, _, Acc) ->
    lists:reverse(Acc).

%% Função que calcula a soma de todos os elementos de uma matriz
soma_matriz(Matrix) ->
    soma_matriz(Matrix, 0).

soma_matriz([], Acc) ->
    Acc;

soma_matriz([Row | Rest], Acc) ->
    soma_matriz(Rest, soma_linha(Row) + Acc).

soma_linha(Row) ->
    soma_linha(Row, 0).

soma_linha([], Acc) ->
    Acc;

soma_linha([Head | Tail], Acc) ->
    soma_linha(Tail, Head + Acc).

%% Função que realiza a busca binária em uma lista ordenada
busca_binaria(Lista, Valor) ->
    busca_binaria(Lista, Valor, 1, length(Lista)).

busca_binaria(_, _, Low, High) when Low > High ->
    not_found;

busca_binaria(Lista, Valor, Low, High) ->
    Meio = (Low + High) div 2,
    case lists:nth(Meio, Lista) of
        MeioValor when MeioValor =:= Valor ->
            {ok, Meio};
        MeioValor when MeioValor < Valor ->
            busca_binaria(Lista, Valor, Meio + 1, High);
        _ ->
            busca_binaria(Lista, Valor, Low, Meio - 1)
    end.
```

Neste código, temos três funções principais:

1. `sequencia_fibonacci/1`: Esta função retorna uma sequência de Fibonacci até um determinado número passado como argumento. Ela utiliza recursão para calcular os valores da sequência.

2. `soma_matriz/1`: Esta função calcula a soma de todos os elementos de uma matriz. Ela utiliza recursão para percorrer a matriz e somar os elementos.

3. `busca_binaria/2`: Esta função realiza uma busca binária em uma lista ordenada. Ela utiliza recursão para dividir a lista ao meio e comparar o valor buscado com o valor do meio. 

Espero que este código atenda às suas expectativas!