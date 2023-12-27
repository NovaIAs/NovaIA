Claro! Aqui está um código complexo em Erlang que realiza uma busca em largura em uma árvore binária:

```erlang
-module(busca_em_largura).
-export([main/0]).

% Definindo a estrutura de uma árvore binária
-record(tree, {value, left, right}).

% Função que realiza a busca em largura
bfs(Tree) ->
    Queue1 = [Tree],
    bfs(Queue1).

% Função auxiliar que realiza a busca em largura
bfs([]) ->
    io:format("Busca em largura concluída.~n");
bfs([Tree | Queue1]) ->
    io:format("Visitando nó com valor ~p.~n", [Tree#tree.value]),
    case Tree#tree.left of
        undefined -> ok;
        Left -> bfs(Queue1 ++ [Left])
    end,
    case Tree#tree.right of
        undefined -> ok;
        Right -> bfs(Queue1 ++ [Right])
    end.

% Função principal que cria uma árvore binária e chama a busca em largura
main() ->
    % Criando a árvore binária
    Node1 = #tree{value = 1},
    Node2 = #tree{value = 2},
    Node3 = #tree{value = 3},
    Node4 = #tree{value = 4},
    Node5 = #tree{value = 5},
    Node6 = #tree{value = 6},
    Node7 = #tree{value = 7},
    Node8 = #tree{value = 8},
    
    Node1#tree.left = Node2,
    Node1#tree.right = Node3,
    Node2#tree.left = Node4,
    Node2#tree.right = Node5,
    Node3#tree.left = Node6,
    Node3#tree.right = Node7,
    Node4#tree.left = Node8,
    
    % Chamando a busca em largura
    bfs(Node1).
```

Explicação do código:
- Primeiro, definimos a estrutura de uma árvore binária usando o registro `tree`, que possui campos para o valor do nó, o nó da esquerda e o nó da direita.
- Em seguida, definimos a função `bfs(Tree)` que realiza a busca em largura. Ela recebe uma árvore binária como entrada e começa com uma fila (Queue1) contendo apenas a árvore raiz.
- A função `bfs([])` é o caso de parada, quando a fila estiver vazia, a busca em largura é concluída.
- A função `bfs([Tree | Queue1])` realiza a busca em largura propriamente dita. Ela imprime o valor do nó atual e, em seguida, verifica se existem nós à esquerda e à direita. Se existirem, eles são adicionados à fila.
- A função `main()` é a função principal que cria uma árvore binária com 8 nós e chama a função `bfs()` com a raiz da árvore.

Espero que esse código atenda às suas expectativas!