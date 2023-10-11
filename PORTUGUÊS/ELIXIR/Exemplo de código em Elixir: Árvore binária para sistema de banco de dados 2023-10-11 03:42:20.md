Claro! Aqui está um exemplo de código em Elixir que cria um sistema de banco de dados simples, utilizando uma estrutura de árvore binária:

```elixir
defmodule BinaryTree do
  defstruct [:left, :value, :right]

  def insert(tree, value) when is_nil(tree) do
    %BinaryTree{left: nil, value: value, right: nil}
  end

  def insert(%BinaryTree{value: tree_value} = tree, value) when value < tree_value do
    %BinaryTree{tree | left: insert(tree.left, value)}
  end

  def insert(%BinaryTree{value: tree_value} = tree, value) when value > tree_value do
    %BinaryTree{tree | right: insert(tree.right, value)}
  end

  def insert(%BinaryTree{value: tree_value} = tree, _value) do
    tree
  end

  def search(%BinaryTree{value: value}, value) do
    value
  end

  def search(%BinaryTree{value: tree_value} = tree, value) when value < tree_value do
    search(tree.left, value)
  end

  def search(%BinaryTree{value: tree_value} = tree, value) when value > tree_value do
    search(tree.right, value)
  end
end

defmodule Main do
  def start do
    tree = nil
    tree = BinaryTree.insert(tree, 5)
    tree = BinaryTree.insert(tree, 3)
    tree = BinaryTree.insert(tree, 8)
    tree = BinaryTree.insert(tree, 2)
    tree = BinaryTree.insert(tree, 4)
    tree = BinaryTree.insert(tree, 7)
    tree = BinaryTree.insert(tree, 9)

    IO.inspect BinaryTree.search(tree, 8)
  end
end

Main.start()
```

Neste código, definimos um módulo chamado `BinaryTree` que representa uma estrutura de árvore binária. Cada nó da árvore é representado por um struct contendo três atributos: `left`, `value` e `right`. 

A função `insert` é responsável por inserir um novo valor na árvore. Se a árvore estiver vazia (representada por `nil`), criamos um novo nó contendo o valor. Caso contrário, comparamos o valor a ser inserido com o valor do nó atual e decidimos se devemos percorrer para a esquerda ou para a direita da árvore, chamando recursivamente a função `insert` no nó correspondente.

A função `search` é responsável por buscar um valor na árvore. Iniciamos a busca a partir da raiz da árvore. Se o valor buscado for igual ao valor do nó atual, retornamos o valor encontrado. Caso contrário, decidimos se devemos buscar na subárvore esquerda ou na subárvore direita, chamando recursivamente a função `search`.

No módulo `Main`, criamos uma árvore binária e inserimos alguns valores. Em seguida, realizamos uma busca pelo valor 8 na árvore e exibimos o resultado.

Este exemplo é apenas uma implementação básica e simplificada de uma árvore binária em Elixir. Você pode expandir e aprimorar essa implementação conforme suas necessidades.