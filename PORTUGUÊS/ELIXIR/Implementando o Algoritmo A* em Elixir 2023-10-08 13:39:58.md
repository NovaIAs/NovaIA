Claro! Aqui está um exemplo de um código em Elixir que implementa um algoritmo de busca A*. O A* é um algoritmo de busca heurística que encontra o caminho mais curto entre dois pontos em um grafo.

```elixir
defmodule AStar do
  defstruct [:start, :goal, :open_list, :closed_list]

  def search(start, goal) do
    state = %__MODULE__{
      start: start,
      goal: goal,
      open_list: [{0, start}],
      closed_list: []
    }

    loop(state)
  end

  defp loop(state) do
    case state.open_list do
      [] ->
        IO.puts "Caminho não encontrado!"
      [{_cost, current} | _] when current == state.goal ->
        IO.puts "Caminho encontrado!"
      [{cost, current} | rest] ->
        children = generate_children(current)
        new_open_list = update_open_list(rest, children, state.closed_list)

        new_state = %__MODULE__{
          start: state.start,
          goal: state.goal,
          open_list: new_open_list,
          closed_list: [current | state.closed_list]
        }

        loop(new_state)
    end
  end

  defp generate_children(current) do
    # Aqui você pode implementar a lógica para gerar os filhos de um determinado nó no grafo
    # Por exemplo, você pode gerar os filhos a partir das arestas do nó atual

    # Exemplo de código fictício para gerar filhos:
    [
      {current + 1, 1},
      {current + 2, 1},
      {current * 2, 2}
    ]
  end

  defp update_open_list(open_list, children, closed_list) do
    # Aqui você pode implementar a lógica para atualizar a lista aberta com os novos filhos gerados
    # Por exemplo, pode verificar se os filhos já estão na lista fechada antes de adicioná-los

    # Exemplo de código fictício para atualizar a lista aberta:
    Enum.filter(children, fn {child, _} -> not (child in closed_list) end) ++ open_list
  end
end

# Testando o algoritmo A*
AStar.search(0, 10)
```

Neste exemplo, a função `search` da struct `AStar` recebe um nó inicial e um nó objetivo como parâmetros. Em seguida, é criado um estado inicial com a lista aberta contendo apenas o nó inicial e a lista fechada vazia. 

O algoritmo então entra em um loop, onde são feitas as seguintes verificações:

- Se a lista aberta está vazia, significa que o caminho não foi encontrado.
- Se o nó atual é igual ao nó objetivo, significa que o caminho foi encontrado.
- Caso contrário, são gerados os filhos do nó atual através da função `generate_children`. Essa função é fictícia neste exemplo, mas você pode implementar a lógica necessária para gerar os filhos de acordo com o seu problema.
- A lista aberta é atualizada com os filhos gerados utilizando a função `update_open_list`. Essa função também é fictícia neste exemplo, e você deve implementá-la de acordo com a sua lógica de atualização da lista aberta.
- O estado é atualizado com a nova lista aberta e o nó atual é adicionado à lista fechada.
- O loop continua até que uma das condições anteriores seja satisfeita.

Este é apenas um exemplo básico de um algoritmo A* em Elixir. Você pode adaptá-lo de acordo com a sua necessidade, implementando a lógica para gerar os filhos e atualizar a lista aberta de acordo com as regras do seu problema específico.