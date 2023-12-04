-module(complexo).
-export([main/0]).

main() ->
    io:format("Bem-vindo ao código complexo em Erlang!~n"),
    io:format("Este código tem como objetivo demonstrar o uso de recursão e pattern matching para calcular o fatorial de um número.~n"),
    io:format("Por favor, digite um número inteiro positivo: "),
    Number = read_integer(),
    Result = factorial(Number),
    io:format("O fatorial de ~w é ~w.~n", [Number, Result]).

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).
    
% Este código começa definindo um módulo chamado "complexo" e exportando a função "main/0".
% A função main() é a função principal que será chamada quando o código for executado.

% Dentro da função main(), mostramos uma mensagem de boas-vindas e explicamos brevemente o propósito do código.

% Em seguida, pedimos ao usuário para digitar um número inteiro positivo e armazenamos o valor digitado na variável "Number".

% Chamamos então a função factorial(Number), que é responsável por calcular o fatorial do número digitado.

% A função factorial/1 é definida utilizando pattern matching. Primeiro, temos um padrão quando o argumento é igual a 0, onde retornamos o valor 1.

% Em seguida, temos um padrão mais genérico, quando o argumento é diferente de 0. Neste caso, multiplicamos o argumento pelo resultado da função factorial(N-1),
% onde N é o argumento atual menos 1. Isso é feito de forma recursiva, até que o argumento seja igual a 0 e o padrão base seja correspondido.

% Por fim, exibimos o resultado do cálculo utilizando a função io:format, mostrando o número digitado pelo usuário e o resultado do fatorial.