Claro, vou gerar um código em Erlang que implementa um algoritmo de busca em largura (BFS) em um grafo direcionado. O código terá comentários explicando cada parte do algoritmo.

```
-module(busca_largura).
-export([bfs/2]).

%% Função principal que realiza a busca em largura
bfs(Graph, StartNode) ->
    %% Cria uma fila vazia e uma lista vazia para armazenar os nós visitados
    Queue = queue:new(),
    Visited = [],

    %% Adiciona o nó inicial à fila e à lista de visitados
    Queue1 = queue:in(StartNode, Queue),
    Visited1 = [StartNode],

    %% Chama a função auxiliar para realizar a busca em largura
    bfs_aux(Graph, Queue1, Visited1).

%% Função auxiliar que realiza a busca em largura
bfs_aux(Graph, Queue, Visited) ->
    %% Verifica se a fila está vazia, ou seja, se todos os nós foram visitados
    case queue:is_empty(Queue) of
        true ->
            %% Se a fila está vazia, retorna a lista de nós visitados
            Visited;
        false ->
            %% Se a fila não está vazia, remove o primeiro nó da fila
            {Node, Queue1} = queue:out(Queue),

            %% Obtém todos os vizinhos do nó atual no grafo
            Neighbors = lists:filter(fun(X) -> lists:member(X, Visited) end, Graph[Node]),

            %% Adiciona os vizinhos à fila e à lista de visitados
            Queue2 = queue:in_list(Neighbors, Queue1),
            Visited2 = lists:append(Visited, Neighbors),

            %% Chama recursivamente a função auxiliar com a nova fila e lista de visitados
            bfs_aux(Graph, Queue2, Visited2)
    end.
```

Neste código, a função `bfs/2` é a função principal que realiza a busca em largura. Ela recebe dois argumentos: o grafo em forma de lista de adjacência (`Graph`) e o nó inicial (`StartNode`).

A função `bfs_aux/3` é a função auxiliar que realiza a busca em largura de fato. Ela recebe três argumentos: o grafo, a fila de nós a serem visitados (`Queue`) e a lista de nós já visitados (`Visited`).

A busca em largura começa criando uma fila vazia (`Queue`) e uma lista vazia de nós visitados (`Visited`).

Em seguida, o nó inicial é adicionado à fila e à lista de visitados.

A função auxiliar é chamada para iniciar a busca em largura.

Na função auxiliar, é verificado se a fila está vazia. Se estiver vazia, significa que todos os nós foram visitados e a lista final de nós visitados é retornada.

Caso contrário, o primeiro nó da fila é removido e seus vizinhos são obtidos a partir do grafo. Os vizinhos que ainda não foram visitados são adicionados à fila e à lista de visitados.

A função auxiliar é então chamada recursivamente com a nova fila e lista de visitados.

O algoritmo continua a buscar em largura até que todos os nós sejam visitados.

Espero que esse código complexo em Erlang atenda às suas expectativas!