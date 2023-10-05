Claro! Aqui está um código complexo em ERLANG que cria uma estrutura de dados e executa uma série de operações sobre ela:

```erlang
-module(complex_code).
-export([main/0]).

% Função para criar uma lista de números primos
create_prime_list(N) ->
    create_prime_list(N, 2, []).

create_prime_list(N, K, Acc) when K > N ->
    lists:reverse(Acc);
create_prime_list(N, K, Acc) ->
    if
        is_prime(K) -> create_prime_list(N, K + 1, [K | Acc]);
        true -> create_prime_list(N, K + 1, Acc)
    end.

% Função para verificar se um número é primo
is_prime(N) ->
    is_prime(N, 2).

is_prime(N, K) when K > trunc(math:sqrt(N)) ->
    true;
is_prime(N, K) when N rem K =:= 0 ->
    false;
is_prime(N, K) ->
    is_prime(N, K + 1).

% Função principal
main() ->
    io:format("Digite um número inteiro positivo: "),
    {ok, [Input]} = io:fread("~d", []),
    PrimeList = create_prime_list(Input),
    io:format("Lista de números primos até ~w:~n~p~n", [Input, PrimeList]).
```

Aqui está uma explicação do código:

1. Definimos um módulo chamado "complex_code" usando a diretiva `-module(complex_code)`.
2. Exportamos a função `main/0` usando a diretiva `-export([main/0])`. Isso permite que a função seja chamada externamente.
3. A função `create_prime_list/1` recebe um número inteiro `N` e retorna uma lista de números primos.
   - Ela chama a função auxiliar `create_prime_list/3` com parâmetros iniciais `N`, `2` e uma lista vazia `[]`.
   - A função auxiliar `create_prime_list/3` utiliza uma estrutura de controle `if` para verificar se cada número é primo.
   - Se um número é primo, ele é adicionado à lista acumuladora `Acc` e a função é chamada recursivamente com o próximo número `K+1`.
   - Se um número não é primo, a função é chamada recursivamente com o próximo número `K+1` sem adicionar o número à lista.
   - Quando `K` excede `N`, a lista acumuladora é revertida usando `lists:reverse/1` e retornada.
4. A função `is_prime/1` recebe um número inteiro `N` e verifica se ele é primo.
   - Ela chama a função auxiliar `is_prime/2` com parâmetros iniciais `N` e `2`.
   - A função auxiliar `is_prime/2` utiliza uma estrutura `if-else` para verificar se `N` é divisível por algum número `K` entre `2` e a raiz quadrada de `N`.
   - Se `N` é divisível por algum `K`, a função retorna `false`. Caso contrário, a função retorna `true`.
5. A função `main/0` é a função principal que será chamada externamente.
   - Ela solicita ao usuário para digitar um número inteiro positivo usando `io:format/2` e `io:fread/2`.
   - O valor de entrada é lido e armazenado em uma única variável `Input`.
   - A função `create_prime_list/1` é chamada com o valor de entrada para obter a lista de números primos.
   - A lista de números primos é impressa na tela usando `io:format/2` com formatação especial.

Esse código cria uma lista de números primos até um número inteiro positivo fornecido pelo usuário, utilizando funções recursivas e estruturas de controle em ERLANG.