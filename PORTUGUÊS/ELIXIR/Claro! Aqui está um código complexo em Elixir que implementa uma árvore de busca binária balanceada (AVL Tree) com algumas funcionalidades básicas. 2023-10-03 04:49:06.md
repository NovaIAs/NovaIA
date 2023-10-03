Claro! Aqui está um código complexo em Elixir que implementa uma árvore de busca binária balanceada (AVL Tree) com algumas funcionalidades básicas. Vou explicar o código passo a passo para facilitar o entendimento:

```elixir
defmodule AVLTree do
  defstruct node: nil, left: nil, right: nil, height: 0
  
  def insert(tree, value) do
    {:noreply, insert_node(tree, value)}
  end
  
  defp insert_node(nil, value) do
    %AVLTree{node: value, height: 1}
  end
  
  defp insert_node(tree = %AVLTree{node: node}, value) do
    case value <=> node do
      :lt ->
        new_left = insert_node(tree.left, value)
        update_height(new_left)
        
        if needs_balancing?(new_left, tree.right) do
          rotate_right(%AVLTree{tree | left: new_left})
        else
          %AVLTree{tree | left: new_left}
        end
        
      :gt ->
        new_right = insert_node(tree.right, value)
        update_height(new_right)
        
        if needs_balancing?(tree.left, new_right) do
          rotate_left(%AVLTree{tree | right: new_right})
        else
          %AVLTree{tree | right: new_right}
        end
        
      :eq ->
        tree  # Caso o valor já exista na árvore, apenas retorna a árvore atual
    end
  end
  
  defp update_height(%AVLTree{} = tree) do
    %AVLTree{tree | height: 1 + max_height(tree.left, tree.right)}
  end
  
  defp max_height(nil, nil), do: 0
  defp max_height(nil, tree), do: tree.height
  defp max_height(tree, nil), do: tree.height
  defp max_height(tree1, tree2), do: max(tree1.height, tree2.height)
  
  defp needs_balancing?(%AVLTree{} = left, %AVLTree{} = right) do
    abs(left.height - right.height) > 1
  end
  
  defp rotate_right(%AVLTree{node: node, left: %AVLTree{} = left, right: right}) do
    %AVLTree{node: left.node, left: left.left, right: %AVLTree{node: node, left: left.right, right: right}, height: 1 + max_height(left.left, %AVLTree{node: node, left: left.right, right: right})}
  end
  
  defp rotate_left(%AVLTree{node: node, left: left, right: %AVLTree{} = right}) do
    %AVLTree{node: right.node, left: %AVLTree{node: node, left: left, right: right.left}, right: right.right, height: 1 + max_height(%AVLTree{node: node, left: left, right: right.left}, right.right)}
  end
end
```

Este código implementa uma árvore de busca binária balanceada chamada AVLTree em Elixir. A AVLTree mantém os nós em uma estrutura definida por `defstruct`, que contém as seguintes propriedades: `node` (valor do nó), `left` (referência para o nó à esquerda), `right` (referência para o nó à direita) e `height` (altura do nó na árvore).

A função `insert` é a função principal de inserção na árvore. Ela recebe a árvore atual e um valor a ser inserido. A função `insert_node` é uma função auxiliar que realiza a inserção do novo nó na árvore.

A função `insert_node` verifica se a árvore atual é nula. Se for nula, cria um novo nó com o valor fornecido. Caso contrário, compara o valor fornecido com o valor do nó atual. Se o valor fornecido for menor, a função chama recursivamente `insert_node` na subárvore esquerda e atualiza a altura da nova subárvore esquerda. Se o valor fornecido for maior, a função chama recursivamente `insert_node` na subárvore direita e atualiza a altura da nova subárvore direita. Se o valor fornecido for igual ao valor do nó atual, a função retorna a árvore atual sem fazer alterações.

A função `update_height` é responsável por atualizar a altura do nó. Ela recebe uma árvore e atualiza sua altura com base nas alturas da subárvore esquerda e da subárvore direita.

A função `max_height` retorna a altura máxima entre duas árvores. Ela é usada para calcular a altura correta após as rotações.

A função `needs_balancing?` verifica se a árvore precisa ser balanceada. Ela compara as alturas da subárvore esquerda e da subárvore direita e retorna true se a diferença for maior que 1.

As funções `rotate_right` e `rotate_left` são responsáveis por realizar as rotações necessárias para balancear a árvore. Elas recebem a árvore atual e retornam a árvore modificada após a rotação.

Espero que isso te ajude a entender melhor como funciona uma AVL Tree em Elixir!