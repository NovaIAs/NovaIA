Claro! Aqui está um exemplo de código complexo em Elixir que realiza uma busca em largura (BFS) em um grafo representado por uma matriz de adjacência:

```elixir
defmodule BFS do
  def busca_largura(grafo, inicio, destino) do
    queue = [inicio]
    visited = [inicio]
    parents = %{}
    path = []

    while !Enum.empty?(queue) do
      vertice = hd(queue)
      queue = tl(queue)

      if vertice == destino do
        path = reconstruir_caminho(parents, destino)
        break
      end

      vizinhos = obter_vizinhos(grafo, vertice)

      Enum.each(vizinhos, fn v ->
        unless List.member?(visited, v) do
          queue = queue ++ [v]
          visited = visited ++ [v]
          parents = Map.put_new(parents, v, vertice)
        end
      end)
    end

    path
  end

  defp obter_vizinhos(grafo, vertice) do
    Enum.filter(grafo[vertice], fn peso -> peso > 0 end)
  end

  defp reconstruir_caminho(parents, destino) do
    path = [destino]
    vertice = destino

    while Map.has_key?(parents, vertice) do
      vertice = parents[vertice]
      path = [vertice | path]
    end

    path
  end
end

# Exemplo de uso
defmodule Main do
  def run do
    grafo = [
      [0, 1, 1, 0, 0],
      [1, 0, 0, 1, 1],
      [1, 0, 0, 0, 0],
      [0, 1, 0, 0, 0],
      [0, 1, 0, 0, 0]
    ]

    inicio = 0
    destino = 4

    caminho = BFS.busca_largura(grafo, inicio, destino)

    IO.inspect(caminho)
  end
end

Main.run()
```

Neste código, temos um módulo chamado `BFS` que implementa a busca em largura em um grafo representado por uma matriz de adjacência. O algoritmo começa pela função `busca_largura`, que recebe o grafo, o vértice de início e o vértice de destino como parâmetros.

A busca em largura é realizada utilizando uma fila (`queue`) para armazenar os vértices a serem visitados. Enquanto a fila não estiver vazia, o algoritmo continua a busca. Para cada vértice retirado da fila, seus vizinhos são verificados. Se um vizinho não tiver sido visitado anteriormente, ele é adicionado à fila, marcado como visitado e seu pai é registrado em um mapa (`parents`).

Quando o vértice de destino é encontrado, o caminho percorrido é reconstruído utilizando o mapa `parents`. O caminho é retornado como resultado da função.

No exemplo de uso, é criado um grafo representado pela matriz de adjacência `grafo`. A busca é iniciada do vértice 0 e busca-se o caminho até o vértice 4. O caminho encontrado é impresso na tela utilizando a função `IO.inspect`.

Espero que esse exemplo atenda às suas expectativas!