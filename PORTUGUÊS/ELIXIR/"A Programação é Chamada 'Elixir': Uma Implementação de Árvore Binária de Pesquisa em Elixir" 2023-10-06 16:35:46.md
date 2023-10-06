Claro! Aqui está um exemplo de código complexo em Elixir que cria uma árvore binária de pesquisa e implementa a função de busca:

```elixir
defmodule BinaryTree do
  defstruct [:value, :left, :right]
  
  def new(value) do
    %BinaryTree{value: value, left: nil, right: nil}
  end
  
  def insert(tree, value) do
    case tree do
      nil -> new(value)
      %BinaryTree{value: v} when value < v -> %{tree | left: insert(tree.left, value)}
      %BinaryTree{value: v} when value > v -> %{tree | right: insert(tree.right, value)}
      _ -> tree
    end
  end
  
  def search(tree, value) do
    case tree do
      nil -> false
      %BinaryTree{value: v} when value < v -> search(tree.left, value)
      %BinaryTree{value: v} when value > v -> search(tree.right, value)
      _ -> true
    end
  end
end

# Exemplo de uso:
tree = BinaryTree.new(5)
tree = BinaryTree.insert(tree, 3)
tree = BinaryTree.insert(tree, 7)
tree = BinaryTree.insert(tree, 1)
tree = BinaryTree.insert(tree, 4)

IO.puts "O valor 4 está na árvore? #{BinaryTree.search(tree, 4)}"
IO.puts "O valor 10 está na árvore? #{BinaryTree.search(tree, 10)}"
```

Neste código, definimos um módulo chamado `BinaryTree` que representa uma árvore binária de pesquisa. A estrutura da árvore é definida usando a diretiva `defstruct`, que nos permite acessar os campos `:value`, `:left` e `:right` de cada nó da árvore.

A função `new/1` é usada para criar um novo nó da árvore com um determinado valor. A função `insert/2` é responsável por inserir um novo valor na árvore de forma adequada, seguindo as regras da árvore binária de pesquisa. A função `search/2` é usada para buscar um valor na árvore e retorna `true` se o valor for encontrado, ou `false` caso contrário.

No exemplo de uso, criamos uma árvore binária de pesquisa com alguns valores e realizamos duas buscas: uma para o valor 4 e outra para o valor 10. O resultado das buscas é impresso usando a função `IO.puts`.

Este código é apenas um exemplo básico de uma árvore binária de pesquisa em Elixir. É possível adicionar mais funcionalidades e recursos conforme necessário.