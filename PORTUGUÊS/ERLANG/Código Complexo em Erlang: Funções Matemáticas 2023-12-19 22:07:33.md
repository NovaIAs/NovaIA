Claro! Aqui está um exemplo de um código complexo em Erlang:

```erlang
-module(complexo).
-export([executar/0]).

% Função para calcular o fatorial de um número
fatorial(0) -> 1;
fatorial(N) when N > 0 -> N * fatorial(N - 1).

% Função para calcular o número de Fibonacci de um número
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N - 1) + fibonacci(N - 2).

% Função para verificar se um número é primo
primo(N) -> primo(N, 2).
primo(N, D) when N =:= D -> true;
primo(N, D) when N rem D =:= 0 -> false;
primo(N, D) -> primo(N, D + 1).

% Função para calcular o quadrado de um número
quadrado(N) -> N * N.

% Função para calcular a média de uma lista de números
media(Lista) -> soma(Lista) / length(Lista).
soma(Lista) -> lists:sum(Lista).

% Função principal que executa todas as operações
executar() ->
    io:format("Digite um número inteiro positivo: "),
    {ok, [Numero]} = io:fread(""),
    io:format("Fatorial de ~w: ~w~n", [Numero, fatorial(Numero)]),
    io:format("Fibonacci de ~w: ~w~n", [Numero, fibonacci(Numero)]),
    io:format("~w é primo? ~w~n", [Numero, primo(Numero)]),
    io:format("Quadrado de ~w: ~w~n", [Numero, quadrado(Numero)]),
    io:format("Digite uma lista de números separados por espaço: "),
    {ok, [Lista]} = io:fread(""),
    Numeros = string:tokens(Lista, " "),
    io:format("Média da lista: ~w~n", [media([list_to_integer(X) || X <- Numeros])]).
```

Neste código, temos várias funções que realizam operações matemáticas. A função `fatorial/1` calcula o fatorial de um número, `fibonacci/1` calcula o número de Fibonacci de um número, `primo/1` verifica se um número é primo, `quadrado/1` calcula o quadrado de um número, `media/1` calcula a média de uma lista de números e `soma/1` realiza a soma dos elementos de uma lista.

A função `executar/0` é a função principal que solicita ao usuário um número inteiro positivo e uma lista de números. Em seguida, ela chama as outras funções para executar as operações desejadas e imprime os resultados na tela.