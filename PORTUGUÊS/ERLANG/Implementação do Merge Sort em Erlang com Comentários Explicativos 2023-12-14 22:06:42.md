Claro! Aqui está um código complexo em Erlang que faz uso de recursão e pattern matching para implementar um algoritmo de ordenação chamado Merge Sort. O código está em português e contém comentários explicativos para facilitar a compreensão:

```erlang
-module(merge_sort).
-export([merge_sort/1]).

% Função principal que recebe uma lista e chama a função de ordenação merge_sort/3
merge_sort(Lista) ->
    merge_sort(Lista, length(Lista)).

% Função auxiliar que implementa o algoritmo de merge sort
merge_sort(Lista, Tamanho) when Tamanho > 1 ->
    Meio = Tamanho div 2,
    {Esquerda, Direita} = lists:split(Meio, Lista),
    % Chama recursivamente a função merge_sort/3 para dividir as sublistas em partes menores
    Merged = merge(merge_sort(Esquerda, Meio), merge_sort(Direita, Tamanho - Meio)),
    Merged;
    
merge_sort(Lista, _) ->
    Lista.

% Função que faz a união ordenada de duas listas
merge([], Lista) ->
    Lista;
merge(Lista, []) ->
    Lista;
merge([{Valor1, _} = Cabeca1 | Cauda1], [{Valor2, _} = Cabeca2 | Cauda2]) when Valor1 =< Valor2 ->
    [Cabeca1 | merge(Cauda1, [{Valor2, _} | Cauda2])];
merge([{Valor1, _} = Cabeca1 | Cauda1], [{Valor2, _} = Cabeca2 | Cauda2]) when Valor1 > Valor2 ->
    [Cabeca2 | merge([{Valor1, _} | Cauda1], Cauda2]).

```

Neste código, a função `merge_sort/1` é a função principal que recebe uma lista e chama a função `merge_sort/3`. Esta função auxiliar implementa o algoritmo de merge sort.

A função `merge_sort/3` recebe uma lista e o tamanho da lista. Ela verifica se o tamanho é maior que 1 e, caso seja, divide a lista em duas partes. Em seguida, chama recursivamente a função `merge_sort/3` para ordenar as sublistas menores. Por fim, chama a função `merge/2` para fazer a união ordenada das duas sublistas e retorna o resultado.

A função `merge/2` é responsável por fazer a união ordenada de duas listas. Ela recebe duas listas e verifica se uma das listas está vazia, retornando a outra lista caso isso aconteça. Caso contrário, compara os valores do primeiro elemento de cada lista e insere o menor valor na lista de saída. Em seguida, chama recursivamente a função `merge/2` com as caudas das listas.

Espero que este código atenda às suas expectativas!