Claro! Aqui está um exemplo de um código complexo em Erlang que envolve o cálculo de números primos:

```erlang
-module(primeiro).

-export([main/0]).

main() ->
    io:format("Digite um número inteiro positivo: "),
    {ok, Input} = io:fread(""),
    case is_integer(Input) of
        true ->
            Number = list_to_integer(Input),
            case is_prime(Number) of
                true ->
                    io:format("~w é um número primo.~n", [Number]);
                false ->
                    io:format("~w não é um número primo.~n", [Number])
            end;
        false ->
            io:format("Entrada inválida. Por favor, digite um número inteiro positivo.~n")
    end,
    main().

is_prime(Number) ->
    case Number of
        0 -> false;
        1 -> false;
        2 -> true;
        _ -> is_prime(Number, 2)
    end.

is_prime(Number, Divisor) ->
    case Divisor of
        Div when Div * Div > Number -> true;
        Div when Number rem Div == 0 -> false;
        _ -> is_prime(Number, Divisor + 1)
    end.
```

Este código em Erlang permite que o usuário digite um número inteiro positivo e verifica se é um número primo ou não. Vou explicar as partes principais do código:

1. O módulo é definido como `primeiro` usando a diretiva `-module` e a lista de funções exportadas é definida usando a diretiva `-export`.

2. A função `main/0` é a função principal que é chamada quando o programa é executado. Ela solicita ao usuário que digite um número inteiro positivo e chama a função `is_prime/1` para verificar se o número é primo ou não.

3. A função `is_prime/1` é responsável por verificar se um número é primo ou não. Ela lida com os casos especiais de 0, 1 e 2, e chama a função `is_prime/2` para números maiores que 2.

4. A função `is_prime/2` é uma função recursiva que verifica se um número é primo ou não, usando divisão iterativa. Ela verifica se o divisor atual é maior que a raiz quadrada do número, caso em que retorna verdadeiro, ou se o número é divisível pelo divisor atual, caso em que retorna falso. Caso contrário, chama-se a si mesma incrementando o divisor em 1.

Espero que este exemplo atenda às suas expectativas. Lembre-se de que você pode modificar e ajustar o código conforme necessário.