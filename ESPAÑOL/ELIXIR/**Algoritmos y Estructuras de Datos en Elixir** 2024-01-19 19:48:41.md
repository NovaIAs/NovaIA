```elixir
defmodule Multiplicar {
  def multiply(a, b) do
    if is_zero(b) do
      0
    else
      a + multiply(a, b - 1)
    end
  end

  def is_zero(n) do
    n == 0
  end
}

defmodule Factorial {
  def factorial(n) do
    if n == 0 do
      1
    else
      n * factorial(n - 1)
    end
  end
}

defmodule CalcularPi {
  def calculate_pi(n) do
    numerador = 4
    denominador = 1
    sum = 0

    for i <- 0..n do
      sum = sum + numerador / denominador
      numerador = numerador * -1
      denominador = denominador + 2
    end

    sum
  end
}

defmodule Fibonacci {
  def fibonacci(n) do
    if n <= 1 do
      n
    else
      fibonacci(n - 1) + fibonacci(n - 2)
    end
  end
}

defmodule OrdenarLista {
  def sort_list(list) do
    if length(list) <= 1 do
      list
    else
      pivot = hd(list)
      left = sort_list(Enum.filter(list, &(&1 < pivot)))
      right = sort_list(Enum.filter(list, &(&1 >= pivot)))
      left ++ [pivot] ++ right
    end
  end
}

defmodule BuscarBinario {
  def binary_search(list, target) do
    low = 0
    high = length(list) - 1

    while low <= high do
      mid = div(low + high, 2)
      guess = list[mid]

      if guess == target do
        {:ok, mid}
      elsif guess < target do
        low = mid + 1
      else
        high = mid - 1
      end
    end

    {:error, :not_found}
  end
}

defmodule ArbolBinario {
  defstruct value: nil, left: nil, right: nil

  def insert(tree, value) do
    if is_nil(tree) do
      %ArbolBinario{value: value}
    else
      if value < tree.value do
        %ArbolBinario{
          value: tree.value,
          left: insert(tree.left, value),
          right: tree.right
        }
      else
        %ArbolBinario{
          value: tree.value,
          left: tree.left,
          right: insert(tree.right, value)
        }
      end
    end
  end

  def search(tree, value) do
    if is_nil(tree) do
      false
    else
      if value == tree.value do
        true
      elsif value < tree.value do
        search(tree.left, value)
      else
        search(tree.right, value)
      end
    end
  end
}

defmodule Grafo {
  defstruct vertices: [], edges: []

  def add_vertex(graph, vertex) do
    if vertex in graph.vertices do
      graph
    else
      %Grafo{
        vertices: graph.vertices ++ [vertex],
        edges: graph.edges
      }
    end
  end

  def add_edge(graph, vertex1, vertex2) do
    if vertex1 not in graph.vertices or vertex2 not in graph.vertices do
      graph
    else
      %Grafo{
        vertices: graph.vertices,
        edges: graph.edges ++ [{vertex1, vertex2}]
      }
    end
  end

  def has_path(graph, vertex1, vertex2) do
    visited = Map.new()

    dfs(graph, vertex1, vertex2, visited)
  end

  defp dfs(graph, vertex, destination, visited) do
    if vertex == destination do
      true
    else
      if vertex in visited do
        false
      else
        Map.put(visited, vertex, true)

        for neighbor <- graph.edges[vertex] do
          if dfs(graph, neighbor, destination, visited) do
            true
          end
        end

        false
      end
    end
  end
}

# Ejemplo de uso

# Multiplicación
IO.puts("Multiplicación: #{Multiplicar.multiply(10, 5)}")

# Factorial
IO.puts("Factorial: #{Factorial.factorial(5)}")

# Cálculo de Pi
IO.puts("Valor aproximado de Pi: #{CalcularPi.calculate_pi(1000)}")

# Fibonacci
IO.puts("Secuencia de Fibonacci: #{Fibonacci.fibonacci(10)}")

# Ordenar lista
list = [5, 3, 1, 2, 4]
IO.puts("Lista ordenada: #{OrdenarLista.sort_list(list)}")

# Búsqueda binaria
list = [1, 3, 5, 7, 9]
target = 7
result = BuscarBinario.binary_search(list, target)

if result == {:ok, index} do
  IO.puts("Elemento encontrado en la posición #{index}")
else
  IO.puts("Elemento no encontrado")
end

# Árbol binario
tree = %ArbolBinario{value: 10}
tree = ArbolBinario.insert(tree, 5)
tree = ArbolBinario.insert(tree, 15)
tree = ArbolBinario.insert(tree, 2)
tree = ArbolBinario.insert(tree, 7)
tree = ArbolBinario.insert(tree, 12)
tree = ArbolBinario.insert(tree, 20)

if ArbolBinario.search(tree, 7) do
  IO.puts("Elemento encontrado en el árbol binario")
else
  IO.puts("Elemento no encontrado en el árbol binario")
end

# Grafo
graph = %Grafo{vertices: [], edges: []}
graph = Grafo.add_vertex(graph, "A")
graph = Grafo.add_vertex(graph, "B")
graph = Grafo.add_vertex(graph, "C")
graph = Grafo.add_vertex(graph, "D")
graph = Grafo.add_edge(graph, "A", "B")
graph = Grafo.add_edge(graph, "B", "C")
graph = Grafo.add_edge(graph, "C", "D")