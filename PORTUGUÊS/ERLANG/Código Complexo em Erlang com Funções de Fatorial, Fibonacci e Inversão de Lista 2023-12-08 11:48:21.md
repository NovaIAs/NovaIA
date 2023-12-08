Claro, vou gerar um código complexo em Erlang para você. Aqui está:

```erlang
-module(complexo).
-export([calcular_fatorial/1, calcular_fibonacci/1, inverter_lista/1]).
 
% Função para calcular o fatorial de um número
calcular_fatorial(N) when N >= 0 ->
    calcular_fatorial(N, 1).
 
calcular_fatorial(0, Acc) -> 
    Acc;
 
calcular_fatorial(N, Acc) when N > 0 ->
    calcular_fatorial(N - 1, N * Acc).
 
% Função para calcular a sequência de Fibonacci
calcular_fibonacci(N) when N >= 0 ->
    calcular_fibonacci(N, 0, 1).
 
calcular_fibonacci(0, Acc1, _) ->
    Acc1;
 
calcular_fibonacci(N, Acc1, Acc2) when N > 0 ->
    calcular_fibonacci(N - 1, Acc2, Acc1 + Acc2).
 
% Função para inverter uma lista
inverter_lista(Lista) ->
    inverter_lista(Lista, []).
 
inverter_lista([], Acc) ->
    Acc;
 
inverter_lista([H|T], Acc) ->
    inverter_lista(T, [H|Acc]).

```

Este código complexo em Erlang contém três funções diferentes. Vou explicar cada uma delas:

1. A função `calcular_fatorial/1` calcula o fatorial de um número dado. Ela utiliza uma técnica recursiva para calcular o fatorial e vai acumulando o resultado em uma variável chamada `Acc`. A função `calcular_fatorial/2` é chamada internamente para realizar a recursão.

2. A função `calcular_fibonacci/1` calcula a sequência de Fibonacci até um determinado número dado. Também utiliza uma técnica recursiva e acumula o resultado em duas variáveis chamadas `Acc1` e `Acc2`. A função `calcular_fibonacci/3` é chamada internamente para realizar a recursão.

3. A função `inverter_lista/1` inverte a ordem dos elementos de uma lista dada. Utiliza uma técnica recursiva e acumula o inverso da lista em uma variável chamada `Acc`. A função `inverter_lista/2` é chamada internamente para realizar a recursão.

Espero que este código complexo em Erlang atenda às suas expectativas!