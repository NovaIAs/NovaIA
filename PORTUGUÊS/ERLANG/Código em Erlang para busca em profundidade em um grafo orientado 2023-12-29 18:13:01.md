Claro, vou gerar um código em Erlang para você. Aqui está um exemplo de um código complexo que implementa um algoritmo de busca em profundidade em um grafo orientado:

```erlang
-module(busca_profundidade).
-export([dfs/2]).

dfs(Grafo, Inicio) ->
    dfs_aux(Grafo, Inicio, []).

dfs_aux(_, [], Visitados) ->
    Visitados;
dfs_aux(Grafo, [Vertice | Resto], Visitados) ->
    case lists:member(Vertice, Visitados) of
        true -> dfs_aux(Grafo, Resto, Visitados);
        false ->
            Novos_Vizinhos = lists:append(lists:drop(length(Grafo), lists:nth(Vertice, Grafo)), Resto),
            dfs_aux(Grafo, lists:usort(Novos_Vizinhos), [Vertice | Visitados])
    end.
```

Neste código, o módulo `busca_profundidade` implementa a função `dfs/2` (depth-first search), que realiza a busca em profundidade em um grafo orientado. 

A função `dfs/2` recebe dois argumentos: o grafo representado como uma lista de listas de adjacências e o vértice de início da busca. A função chama a função auxiliar `dfs_aux/3`, passando o grafo, uma lista contendo apenas o vértice de início e uma lista vazia para armazenar os vértices visitados.

A função `dfs_aux/3` é responsável por realizar a busca em profundidade. Primeiro, ela verifica se a lista de vértices a serem visitados está vazia. Se estiver vazia, retorna a lista de vértices visitados. Caso contrário, ela verifica se o vértice atual já foi visitado. Se sim, chama recursivamente `dfs_aux/3`, passando o restante dos vértices a serem visitados e a lista de vértices visitados até o momento.

Se o vértice atual não foi visitado, a função obtém os vizinhos do vértice atual e adiciona-os à lista de vértices a serem visitados. Em seguida, chama recursivamente `dfs_aux/3`, passando a nova lista de vértices a serem visitados, ordenada e sem duplicatas, e a lista de vértices visitados atualizada com o vértice atual.

Espero que este exemplo atenda aos seus requisitos de complexidade e originalidade.