Claro! Aqui está um exemplo de um código complexo em Elixir que implementa um algoritmo de busca em largura para encontrar o menor caminho entre dois pontos em um grafo:

```elixir
defmodule BuscaEmLargura do
  def caminho_mais_curto(grafo, inicio, fim) do
    # Cria uma fila vazia para armazenar os vértices a serem visitados
    fila = [inicio]

    # Cria um mapa vazio para armazenar os predecessores de cada vértice
    predecessores = %{inicio => nil}

    # Enquanto a fila não estiver vazia
    while !Enum.empty?(fila) do
      # Remove o primeiro vértice da fila
      vertice = hd(fila)
      fila = tl(fila)

      # Se encontrou o destino, constrói o caminho a partir dos predecessores
      if vertice == fim do
        caminho = construir_caminho(predecessores, fim, [])
        return Enum.reverse(caminho)
      end

      # Para cada vértice adjacente ao vértice atual
      Enum.each(adjacentes(grafo, vertice), fn adjacente ->
        # Se o vértice adjacente ainda não foi visitado
        if !Map.has_key?(predecessores, adjacente) do
          # Adiciona o vértice adjacente à fila e define seu predecessor
          fila = fila ++ [adjacente]
          predecessores = Map.put_new(predecessores, adjacente, vertice)
        end
      end)
    end

    # Se não encontrou nenhum caminho entre os vértices, retorna nil
    nil
  end

  defp adjacentes(grafo, vertice) do
    # Retorna a lista de vértices adjacentes ao vértice atual
    # implemente sua própria lógica de busca em largura aqui
    # para obter os vértices adjacentes
    # por exemplo, pode-se usar a função Enum.filter para filtrar
    # os vértices que estão conectados ao vértice atual no grafo
    # e retornar apenas esses vértices
    # return Enum.filter(grafo, fn {v, _} -> v == vertice end)
    ...
  end

  defp construir_caminho(predecessores, vertice, caminho) do
    # Recursivamente constrói o caminho a partir dos predecessores
    if vertice == nil do
      caminho
    else
      construir_caminho(predecessores, Map.get(predecessores, vertice), [vertice | caminho])
    end
  end
end
```

Este código implementa a função `caminho_mais_curto/3` que recebe um grafo, um vértice de início e um vértice de fim, e retorna o caminho mais curto entre esses dois vértices.

O algoritmo de busca em largura utiliza uma fila para armazenar os vértices a serem visitados. Ele visita cada vértice adjacente ao vértice atual, adicionando-o à fila se ainda não foi visitado. A função `adjacentes/2` é responsável por retornar os vértices adjacentes ao vértice atual no grafo.

A função `construir_caminho/3` é uma função auxiliar que constrói o caminho a partir dos predecessores, percorrendo recursivamente o mapa de predecessores até chegar ao vértice de início.

Observe que a implementação da função `adjacentes/2` não está presente neste exemplo, pois varia de acordo com a estrutura do grafo utilizado. Você deve implementá-la de acordo com as suas necessidades.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, sinta-se à vontade para perguntar.