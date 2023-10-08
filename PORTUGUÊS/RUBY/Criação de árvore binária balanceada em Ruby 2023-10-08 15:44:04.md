Claro! Aqui está um exemplo de código em Ruby que cria uma árvore binária balanceada. Essa árvore é criada a partir de um array de elementos e é balanceada de forma a garantir que a diferença entre as alturas das subárvores esquerda e direita seja no máximo 1.

```ruby
class Node
  attr_accessor :data, :left, :right
  
  def initialize(data)
    @data = data
    @left = nil
    @right = nil
  end
end

class BinaryTree
  def initialize(array)
    @root = build_tree(array)
  end
  
  def build_tree(array)
    return nil if array.empty?
    sorted_array = array.uniq.sort
    mid = (sorted_array.length - 1) / 2
    
    root = Node.new(sorted_array[mid])
    root.left = build_tree(sorted_array[0...mid])
    root.right = build_tree(sorted_array[mid+1..-1])

    return root
  end
  
  def insert(value, current = @root)
    return Node.new(value) if current.nil?
    
    if value < current.data
      current.left = insert(value, current.left)
    elsif value > current.data
      current.right = insert(value, current.right)
    end
    
    return current
  end
  
  def pretty_print(node = @root, prefix = '', is_left = true)
    pretty_print(node.right, "#{prefix}#{is_left ? '│   ' : '    '}", false) if node.right
    puts "#{prefix}#{is_left ? '└── ' : '┌── '}#{node.data}"
    pretty_print(node.left, "#{prefix}#{is_left ? '    ' : '│   '}", true) if node.left
  end
end

# Exemplo de uso:
array = [7, 3, 2, 5, 4, 6, 9, 8, 10]
tree = BinaryTree.new(array)
tree.insert(1)
tree.pretty_print
```

Explicação do código:

1. Primeiro, criamos uma classe `Node` que representa cada nó da árvore. Cada nó contém um dado (`data`), assim como referências para seus nós filhos esquerdo (`left`) e direito (`right`).

2. Em seguida, criamos a classe `BinaryTree` que contém a lógica para construir e balancear a árvore binária.

3. No método `build_tree`, recebemos um array como entrada. Caso o array esteja vazio, retornamos `nil`. Caso contrário, ordenamos o array, obtendo assim uma ordem crescente dos elementos. Em seguida, encontramos o elemento central do array (arredondando para baixo em caso de número ímpar de elementos) e criamos um nó com esse valor como raiz. Chamamos recursivamente o método `build_tree` para os elementos antes e depois do elemento central, atribuindo os resultados como nós filhos esquerdo e direito do nó raiz, respectivamente.

4. O método `insert` permite inserir um novo valor na árvore. Recebe um parâmetro `value` que representa o valor a ser inserido e um parâmetro opcional `current` que indica o nó em que se está fazendo a inserção (por padrão, inicia-se na raiz). O método segue percorrendo a árvore de forma recursiva, comparando o valor a ser inserido com o valor do nó atual. Se o valor for menor, a inserção é feita no nó filho esquerdo. Se for maior, a inserção é feita no nó filho direito. A função retorna o nó atualizado após a inserção.

5. O método `pretty_print` é responsável por imprimir a árvore de forma compreensível. Ele recebe um parâmetro `node` que indica o nó atual a ser impresso, um parâmetro `prefix` que representa o prefixo a ser adicionado antes da impressão do nó e um parâmetro `is_left` que indica se o nó atual é filho esquerdo do seu pai. O método percorre a árvore em ordem inversa (do nó direito para o nó esquerdo) e imprime cada nó no formato visual de uma árvore binária, utilizando caracteres especiais para representar a conexão entre os nós.

6. Por fim, temos um exemplo de uso da classe `BinaryTree`. Criamos um array de exemplo e construímos a árvore a partir desse array. Em seguida, inserimos o valor 1 na árvore e imprimimos a árvore utilizando o método `pretty_print`.