Claro! Aqui está um exemplo de código em Elixir que implementa um algoritmo de busca em largura (BFS) em um grafo não direcionado. Vou explicar cada parte do código para que você possa entender como funciona:

```elixir
defmodule Grafo do
  defstruct vertices: %{}
end

defmodule Vertice do
  defstruct nome: "", vizinhos: []
end

def bfs(grafo, inicio) do
  queue = [inicio]
  visited = MapSet.new([inicio])

  while !Enum.empty?(queue) do
    vertice = hd(queue)
    IO.puts("Visitando o vértice #{vertice.nome}")
    queue = tl(queue)

    for vizinho <- vertice.vizinhos do
      if !MapSet.member?(visited, vizinho) do
        visited = MapSet.put(visited, vizinho)
        queue = queue ++ [vizinho]
      end
    end
  end
end

def run do
  vertice_a = %Vertice{nome: "A", vizinhos: []}
  vertice_b = %Vertice{nome: "B", vizinhos: []}
  vertice_c = %Vertice{nome: "C", vizinhos: []}
  vertice_d = %Vertice{nome: "D", vizinhos: []}
  vertice_e = %Vertice{nome: "E", vizinhos: []}
  
  vertice_a.vizinhos = [vertice_b, vertice_c]
  vertice_b.vizinhos = [vertice_a, vertice_d]
  vertice_c.vizinhos = [vertice_a, vertice_d]
  vertice_d.vizinhos = [vertice_b, vertice_c, vertice_e]
  vertice_e.vizinhos = [vertice_d]
  
  grafo = %Grafo{vertices: %{
    "A" => vertice_a,
    "B" => vertice_b,
    "C" => vertice_c,
    "D" => vertice_d,
    "E" => vertice_e
  }}
  
  bfs(grafo, vertice_a)
end

run()
```

Neste código, temos duas definições de structs: `Grafo` e `Vertice`. A struct `Grafo` representa o grafo em si, e possui um campo `vertices` que é um mapa onde a chave é o nome do vértice e o valor é a struct `Vertice`.

A struct `Vertice` representa cada vértice do grafo e possui dois campos: `nome` e `vizinhos`. O campo `nome` armazena o nome do vértice, enquanto o campo `vizinhos` é uma lista que contém as referências para os vértices vizinhos.

A função `bfs` implementa o algoritmo de busca em largura. Ela recebe como argumento o grafo e o vértice de partida. A função utiliza uma fila (`queue`) para armazenar os vértices a serem visitados e um conjunto (`visited`) para rastrear os vértices já visitados.

O laço `while` executa até que não haja mais vértices na fila. Em cada iteração, o vértice da frente da fila é removido e visitado. Em seguida, são percorridos os vizinhos desse vértice e, se algum vizinho ainda não tiver sido visitado, ele é adicionado à fila e ao conjunto de visitados.

Por fim, a função `run` cria um grafo de exemplo com cinco vértices (A, B, C, D, E) e define suas conexões. Em seguida, chama a função `bfs` passando o vértice A como ponto de partida.

Espero que este exemplo ajude a ilustrar como implementar um algoritmo de busca em largura em Elixir!