Claro! Aqui está um código complexo em Erlang que utiliza algumas funcionalidades avançadas da linguagem:

```erlang
-module(complexo).

-export([iniciar/0]).

% Função principal que inicia o programa
iniciar() ->
    io:format("Bem-vindo ao programa complexo!~n"),
    io:format("Por favor, insira um número inteiro positivo: "),
    {ok, Numero} = io:fread(""),
    case validar_numero(Numero) of
        true ->
            Resultado = calcular_fatorial(Numero),
            io:format("O fatorial de ~p é ~p.~n", [Numero, Resultado]);
        false ->
            io:format("Número inválido! Por favor, insira um número inteiro positivo.~n")
    end.

% Função que valida se o número é um inteiro positivo
validar_numero(Numero) ->
    case string:to_integer(Numero) of
        {ok, Inteiro} when Inteiro > 0 ->
            true;
        _ ->
            false
    end.

% Função que calcula o fatorial de um número
calcular_fatorial(0) ->
    1;
calcular_fatorial(N) ->
    N * calcular_fatorial(N - 1).
```

Explicação do código:

1. O código começa com a definição do módulo `complexo` e a exportação da função `iniciar/0`.
2. A função `iniciar/0` é a função principal que inicia o programa.
3. Ela exibe uma mensagem de boas-vindas e solicita ao usuário que insira um número inteiro positivo.
4. Em seguida, a função `validar_numero/1` é chamada para validar se o número inserido é válido.
5. Se o número for válido, a função `calcular_fatorial/1` é chamada para calcular o fatorial do número.
6. O resultado é exibido na tela.
7. Se o número for inválido, uma mensagem de erro é exibida.
8. A função `validar_numero/1` verifica se o número é um inteiro positivo convertendo a entrada para inteiro usando `string:to_integer/1`.
9. Se a conversão for bem-sucedida e o número for maior que 0, a função retorna `true`. Caso contrário, retorna `false`.
10. A função `calcular_fatorial/1` calcula o fatorial de um número.
11. Se o número for 0, o resultado é 1.
12. Caso contrário, o fatorial é calculado multiplicando o número pelo fatorial do número anterior (N - 1).

Espero que este código atenda às suas expectativas!