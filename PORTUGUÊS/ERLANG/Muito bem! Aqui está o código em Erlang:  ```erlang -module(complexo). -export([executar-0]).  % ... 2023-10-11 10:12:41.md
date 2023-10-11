Claro! Aqui está um código complexo em Erlang que utiliza o módulo `lists` para manipulação de listas e a função `io:format/2` para exibir resultados:

```erlang
-module(complexo).
-export([executar/0]).

% Função auxiliar para verificar se um número é par ou ímpar
par_impar(Numero) ->
    case Numero rem 2 of
        0 -> par;
        1 -> impar
    end.

% Função principal que executa o código complexo
executar() ->
    Lista = [1, 2, 3, 4, 5],
    Resultado = lists:map(fun(X) ->
                               {X, par_impar(X)}
                           end, Lista),
    exibir_resultado(Resultado).

% Função auxiliar para exibir o resultado
exibir_resultado(Resultado) ->
    lists:foreach(fun({Valor, ParImpar}) ->
                         io:format("O número ~w é ~s.~n", [Valor, ParImpar])
                 end, Resultado).
```

Este código em Erlang define um módulo chamado `complexo` que possui uma função `executar/0`. A função `executar/0` realiza as seguintes etapas:

1. Cria uma lista chamada `Lista` com os números [1, 2, 3, 4, 5].
2. Utiliza a função `lists:map/2` para aplicar uma função anônima a cada elemento da lista `Lista`. A função anônima retorna uma tupla contendo o número e uma string indicando se é par ou ímpar.
3. Armazena o resultado da função `lists:map/2` na variável `Resultado`.
4. Chama a função `exibir_resultado/1` passando o `Resultado` como argumento.

A função `exibir_resultado/1` percorre cada elemento da lista `Resultado` e utiliza a função `io:format/2` para exibir o número e se ele é par ou ímpar na saída padrão.

Para executar o código, é necessário compilar o arquivo `complexo.erl` e chamar a função `complexo:executar().` no console Erlang.

Espero que este exemplo atenda às suas expectativas! Se precisar de mais ajuda ou tiver alguma dúvida, estou à disposição!