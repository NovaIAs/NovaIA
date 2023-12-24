Claro! Aqui está um exemplo de código complexo em Elixir, que implementa um algoritmo de busca em largura (BFS) em um grafo direcionado:

```elixir
defmodule Graph do
  defstruct nodes: %{}

  def new(nodes) do
    %__MODULE__{nodes: nodes}
  end

  def add_edge(graph, from, to) do
    edges = Map.get(graph.nodes, from, [])
    updated_edges = edges ++ [to]
    %{graph | nodes: Map.put(graph.nodes, from, updated_edges)}
  end

  def bfs(graph, start_node) do
    queue = [start_node]
    visited = [start_node]
    result = []

    while queue do
      current_node = List.first(queue)
      queue = List.delete(queue, current_node)
      result = result ++ [current_node]

      edges = Map.get(graph.nodes, current_node, [])

      new_neighbors = Enum.reject(edges, &List.member?(visited, &1))
      visited = visited ++ new_neighbors
      queue = queue ++ new_neighbors
    end

    result
  end
end

defmodule Main do
  def run do
    graph = Graph.new(%{
      "A" => ["B", "C"],
      "B" => ["C", "D"],
      "C" => ["D", "E"],
      "D" => ["E"],
      "E" => []
    })

    start_node = "A"
    result = Graph.bfs(graph, start_node)

    IO.inspect(result)
  end
end

Main.run()
```

Explicação do código:
- A estrutura `Graph` é definida como uma struct, que armazena um mapa de nós e suas arestas.
- A função `new/1` cria uma nova instância do grafo, recebendo um mapa de nós como parâmetro.
- A função `add_edge/3` adiciona uma nova aresta ao grafo, atualizando o mapa de nós.
- A função `bfs/2` implementa o algoritmo de busca em largura. Ela recebe o grafo e o nó inicial como parâmetros.
- Durante a busca em largura, um fila (`queue`) é utilizada para controlar os nós a serem visitados.
- A cada iteração, o primeiro nó da fila é removido e seus vizinhos não visitados são adicionados à fila e marcados como visitados.
- O resultado final é uma lista contendo a ordem em que os nós foram visitados.

No exemplo acima, o grafo possui os nós "A", "B", "C", "D" e "E", com as seguintes arestas:
- A -> B, A -> C
- B -> C, B -> D
- C -> D, C -> E
- D -> E
- E -> (nenhuma aresta)

A busca em largura começa no nó "A" e retorna a ordem em que os nós foram visitados: ["A", "B", "C", "D", "E"].