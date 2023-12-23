Claro! Aqui está um exemplo de um código complexo em Erlang:

-module(complexo).
-export([fibonacci/1]).

% Função para calcular o n-ésimo número da sequência de Fibonacci
fibonacci(N) ->
    fibonacci(N, 0, 1).

fibonacci(0, A, _) ->
    A;
fibonacci(N, A, B) ->
    Next = A + B,
    fibonacci(N - 1, B, Next).

% Função para verificar se um número é primo
is_prime(N) ->
    is_prime(N, 2).

is_prime(1, _) ->
    false;
is_prime(N, D) when N =:= D ->
    true;
is_prime(N, D) when N rem D =:= 0 ->
    false;
is_prime(N, D) ->
    is_prime(N, D + 1).

% Função para calcular o fatorial de um número
fatorial(N) ->
    fatorial(N, 1).

fatorial(0, Acc) ->
    Acc;
fatorial(N, Acc) ->
    fatorial(N - 1, N * Acc).

% Função para inverter uma lista
inverter_lista(List) ->
    inverter_lista(List, []).

inverter_lista([], Acc) ->
    Acc;
inverter_lista([H|T], Acc) ->
    inverter_lista(T, [H|Acc]).

% Função para encontrar o elemento mínimo em uma lista
minimo(List) ->
    minimo(List, hd(List)).

minimo([], Min) ->
    Min;
minimo([H|T], Min) when H < Min ->
    minimo(T, H);
minimo([_|T], Min) ->
    minimo(T, Min).

% Função para verificar se uma lista é palíndrome
is_palindrome(List) ->
    List =:= lists:reverse(List).

% Função para calcular a média de uma lista de números
media(List) ->
    [Sum, Count] = lists:foldl(fun(X, [S, C]) -> [X + S, C + 1] end, [0, 0], List),
    Sum / Count.

% Função para ordenar uma lista em ordem crescente
ordenar(List) ->
    lists:sort(List).

% Função para verificar se todos os elementos de uma lista satisfazem uma condição
todos(List, Fun) ->
    lists:all(Fun, List).

% Função para verificar se algum elemento de uma lista satisfaz uma condição
algum(List, Fun) ->
    lists:any(Fun, List).

% Função principal
main() ->
    io:format("Digite um número para calcular o n-ésimo número da sequência de Fibonacci: "),
    N = io:get_line(""),
    Result1 = fibonacci(list_to_integer(N)),
    io:format("O resultado é: ~p~n", [Result1]),
    
    io:format("Digite um número para verificar se é primo: "),
    M = io:get_line(""),
    Result2 = is_prime(list_to_integer(M)),
    io:format("O resultado é: ~p~n", [Result2]),
    
    io:format("Digite um número para calcular o fatorial: "),
    P = io:get_line(""),
    Result3 = fatorial(list_to_integer(P)),
    io:format("O resultado é: ~p~n", [Result3]),
    
    io:format("Digite uma lista para inverter: "),
    Q = io:get_line(""),
    Result4 = inverter_lista(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Q, " "))),
    io:format("O resultado é: ~p~n", [Result4]),
    
    io:format("Digite uma lista de números para encontrar o mínimo: "),
    R = io:get_line(""),
    Result5 = minimo(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(R, " "))),
    io:format("O resultado é: ~p~n", [Result5]),
    
    io:format("Digite uma lista para verificar se é palíndrome: "),
    S = io:get_line(""),
    Result6 = is_palindrome(list_to_atom(S)),
    io:format("O resultado é: ~p~n", [Result6]),
    
    io:format("Digite uma lista de números para calcular a média: "),
    T = io:get_line(""),
    Result7 = media(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(T, " "))),
    io:format("O resultado é: ~p~n", [Result7]),
    
    io:format("Digite uma lista de números para ordenar: "),
    U = io:get_line(""),
    Result8 = ordenar(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(U, " "))),
    io:format("O resultado é: ~p~n", [Result8]),
    
    io:format("Digite uma lista de números para verificar se todos satisfazem uma condição: "),
    V = io:get_line(""),
    Result9 = todos(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(V, " ")), fun(X) -> X > 0 end),
    io:format("O resultado é: ~p~n", [Result9]),
    
    io:format("Digite uma lista de números para verificar se algum satisfaz uma condição: "),
    W = io:get_line(""),
    Result10 = algum(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(W, " ")), fun(X) -> X < 0 end),
    io:format("O resultado é: ~p~n", [Result10]).