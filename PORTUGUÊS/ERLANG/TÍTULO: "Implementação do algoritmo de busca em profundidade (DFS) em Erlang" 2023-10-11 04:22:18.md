Claro! Aqui está um código em Erlang que implementa um algoritmo de busca em profundidade (DFS) em um grafo direcionado. O algoritmo percorre o grafo, visitando todos os vértices e arestas, usando uma pilha para guardar os vértices a serem visitados.

```erlang
-module(dfs).

-export([dfs/2]).

dfs(Graph, Start) ->
    Visited = sets:new(),
    Stack = [Start],
    dfs(Graph, Visited, Stack).

dfs(_, _, []) -> ok;  % pilha vazia, termina a busca
dfs(Graph, Visited, [Current | Rest]) ->
    case sets:is_element(Current, Visited) of
        true -> dfs(Graph, Visited, Rest);  % vértice já visitado, continua a busca
        false ->
            io:format("Visitando vértice ~p~n", [Current]),
            NewVisited = sets:add_element(Current, Visited),
            Neighbors = get_neighbors(Graph, Current),
            NextVertices = lists:reverse(Neighbors) ++ Rest,
            dfs(Graph, NewVisited, NextVertices)
    end.

get_neighbors(Graph, Vertex) ->
    case dict:find(Vertex, Graph) of
        {ok, Neighbors} -> Neighbors;
        error -> []
    end.
```

Explicação do código:

1. Primeiro, definimos o módulo `dfs` e exportamos a função `dfs/2` para que ela possa ser usada fora do módulo.

2. A função `dfs/2` recebe um grafo representado como um dicionário e um vértice de partida. O dicionário mapeia cada vértice para sua lista de vizinhos.

3. Inicialmente, criamos um conjunto vazio `Visited` para armazenar os vértices visitados e uma pilha `Stack` com o vértice de partida.

4. Chamamos a função auxiliar `dfs/3`, passando o grafo, o conjunto de vértices visitados e a pilha.

5. A função `dfs/3` tem dois casos base:
   - Se a pilha estiver vazia, significa que todos os vértices foram visitados, então o algoritmo termina retornando `ok`.
   - Se o vértice atual já foi visitado, continuamos a busca removendo-o da pilha e chamando recursivamente `dfs/3`.

6. No caso recursivo, primeiro verificamos se o vértice atual já foi visitado usando a função `sets:is_element/2`. Se já foi visitado, removemos o vértice atual da pilha e chamamos recursivamente `dfs/3`.

7. Se o vértice atual não foi visitado, imprimimos uma mensagem informando que estamos visitando o vértice e adicionamos-o ao conjunto de vértices visitados.

8. Em seguida, obtemos a lista de vizinhos do vértice atual usando a função `get_neighbors/2`. Essa função consulta o dicionário `Graph` e retorna a lista de vizinhos do vértice ou uma lista vazia se o vértice não estiver presente no dicionário.

9. Para continuar a busca, precisamos empilhar os vizinhos na ordem inversa, pois a busca em profundidade visita primeiro os vizinhos mais recentemente adicionados. Para isso, usamos `lists:reverse/1` para inverter a ordem dos vizinhos e concatenamos essa lista com a pilha atual.

10. Finalmente, chamamos recursivamente `dfs/3` passando o grafo atualizado, o conjunto de vértices visitados atualizado e a nova pilha de vértices.

Espero que este código atenda às suas expectativas!