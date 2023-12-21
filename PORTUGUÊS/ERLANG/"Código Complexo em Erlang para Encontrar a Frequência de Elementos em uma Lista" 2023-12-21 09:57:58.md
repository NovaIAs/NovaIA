Claro! Aqui está um código complexo em Erlang, que é um pouco grande e diferenciado, e está completamente em português. Vou explicar o código passo a passo para você entender melhor:

```erlang
-module(complexo).
-export([encontrar_frequencia/1]).

%% Função auxiliar para contar a frequência de um elemento em uma lista
contar_elementos(_, [], Contador) -> Contador;
contar_elementos(Elemento, [Cabeça|Cauda], Contador) ->
    if
        Elemento == Cabeça -> contar_elementos(Elemento, Cauda, Contador + 1);
        true -> contar_elementos(Elemento, Cauda, Contador)
    end.

%% Função principal para encontrar a frequência dos elementos em uma lista
encontrar_frequencia(Lista) ->
    Frequencias = sets:to_list(sets:from_list(Lista)),  % Remove elementos duplicados da lista
    FrequenciasContadas = [{Elemento, contar_elementos(Elemento, Lista, 0)} || Elemento <- Frequencias],
    lists:sort(FrequenciasContadas, fun({_, Contagem1}, {_, Contagem2}) -> Contagem2 < Contagem1 end).
```

A função `encontrar_frequencia/1` é a função principal que recebe uma lista como entrada e retorna uma lista de pares contendo cada elemento da lista e a sua frequência. Por exemplo, se chamarmos `encontrar_frequencia([1, 2, 1, 3, 2, 2])`, o resultado será `[{1, 2}, {2, 3}, {3, 1}]`, indicando que o número 1 aparece 2 vezes, o número 2 aparece 3 vezes e o número 3 aparece 1 vez na lista.

A função auxiliar `contar_elementos/3` é responsável por contar a frequência de um determinado elemento em uma lista. Ela utiliza recursão para percorrer a lista e incrementa um contador sempre que encontra o elemento desejado. No final, ela retorna o valor do contador.

No código principal, a lista de entrada é convertida para um conjunto (`sets:from_list(Lista)`) para remover elementos duplicados. Em seguida, a função `contar_elementos/3` é chamada para cada elemento do conjunto, gerando uma lista de pares contendo cada elemento e a sua contagem de frequência. Essa lista é então ordenada em ordem decrescente de contagem de frequência utilizando a função de comparação passada para `lists:sort/2`.

Espero que este código atenda às suas especificações e seja útil para você!