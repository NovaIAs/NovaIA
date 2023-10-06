Claro! Aqui está um código complexo em Erlang:

```erlang
-module(complex_code).
-export([main/0]).

% Função recursiva para calcular o fatorial de um número
factorial(0) -> 1;
factorial(N) ->
    N * factorial(N-1).

% Função auxiliar para verificar se um número é par ou ímpar
is_even(Number) ->
    if
        Number rem 2 =:= 0 ->
            io:format("~w é um número par.~n", [Number]);
        true ->
            io:format("~w é um número ímpar.~n", [Number])
    end.

% Função principal
main() ->
    io:format("Bem-vindo ao código complexo em Erlang!~n"),
    io:format("Por favor, insira um número inteiro positivo: "),
    Number = io:get_line(""),
    case string:to_integer(Number) of
        {ok, N} when N >= 0 ->
            io:format("O fatorial de ~w é ~w.~n", [N, factorial(N)]),
            is_even(N);
        _ ->
            io:format("Número inválido. Por favor, tente novamente.~n")
    end.
```

Explicação:

O código acima é um exemplo de um programa em Erlang que solicita ao usuário um número inteiro positivo, calcula o fatorial desse número e verifica se o número é par ou ímpar. Ele usa algumas funcionalidades básicas da linguagem Erlang, como funções, estruturas de controle e chamadas de entrada/saída.

O módulo `complex_code` é definido usando a diretiva `-module` e exporta a função `main/0` usando a diretiva `-export`.

A função `factorial/1` é uma função recursiva que calcula o fatorial de um número. Ela usa a cláusula `factorial(0) -> 1` como caso base, quando o número é zero, retornando 1. Em seguida, na cláusula `factorial(N) -> N * factorial(N-1)`, o fatorial é calculado multiplicando o número pelo fatorial do número anterior.

A função `is_even/1` é uma função auxiliar que verifica se um número é par ou ímpar. Ela usa a estrutura `if...end` para verificar se o número é divisível por 2 (par) ou não (ímpar) e imprime a mensagem correspondente usando `io:format`.

A função `main/0` é a função principal do programa. Ela imprime uma mensagem de boas-vindas e solicita ao usuário um número inteiro positivo usando `io:format` e `io:get_line`. Em seguida, a entrada é convertida para um número usando `string:to_integer` e é feita uma verificação usando `case` e `when`.

Se o número for um inteiro positivo, o fatorial é calculado chamando a função `factorial` e é exibida a mensagem com o resultado. Em seguida, a função `is_even` é chamada para verificar se o número é par ou ímpar.

Se o número não for válido (negativo ou não inteiro), é exibida uma mensagem de erro.

Espero que este código complexo em Erlang atenda às suas expectativas!