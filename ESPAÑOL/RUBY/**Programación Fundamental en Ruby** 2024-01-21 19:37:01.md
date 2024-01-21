```ruby
# Crear una función recursiva para calcular el factorial de un número

def factorial(n)
  if n == 0
    return 1
  else
    return n * factorial(n-1)
  end
end

# Crear una función para encontrar el número más grande en una lista

def max(list)
  if list.empty?
    return nil
  else
    max_value = list[0]
    for i in 1...list.length
      if list[i] > max_value
        max_value = list[i]
      end
    end
    return max_value
  end
end

# Crear una función para invertir una cadena de caracteres

def reverse_string(str)
  if str.empty?
    return ""
  else
    return str[-1] + reverse_string(str[0...-1])
  end
end

# Crear una función para encontrar la posición de un elemento en una lista

def find_index(list, element)
  for i in 0...list.length
    if list[i] == element
      return i
    end
  end
  return -1
end

# Crear una función para ordenar una lista de números en orden ascendente

def sort_numbers(list)
  if list.length <= 1
    return list
  else
    pivot = list[0]
    less_than_pivot = []
    greater_than_pivot = []
    for i in 1...list.length
      if list[i] < pivot
        less_than_pivot << list[i]
      else
        greater_than_pivot << list[i]
      end
    end
    return sort_numbers(less_than_pivot) + [pivot] + sort_numbers(greater_than_pivot)
  end
end

# Crear una función para crear un árbol binario de búsqueda

class Node
  attr_accessor :value, :left, :right

  def initialize(value)
    @value = value
    @left = nil
    @right = nil
  end
end

class BinarySearchTree
  attr_accessor :root

  def initialize
    @root = nil
  end

  def insert(value)
    if @root == nil
      @root = Node.new(value)
    else
      insert_helper(@root, value)
    end
  end

  def insert_helper(node, value)
    if value < node.value
      if node.left == nil
        node.left = Node.new(value)
      else
        insert_helper(node.left, value)
      end
    else
      if node.right == nil
        node.right = Node.new(value)
      else
        insert_helper(node.right, value)
      end
    end
  end

  def find(value)
    if @root == nil
      return nil
    else
      find_helper(@root, value)
    end
  end

  def find_helper(node, value)
    if node.value == value
      return node
    elsif value < node.value
      if node.left == nil
        return nil
      else
        find_helper(node.left, value)
      end
    else
      if node.right == nil
        return nil
      else
        find_helper(node.right, value)
      end
    end
  end
end

# Main program

# Calcular el factorial de 5
puts "Factorial de 5: #{factorial(5)}"

# Encontrar el número más grande en una lista
list = [1, 3, 5, 7, 9]
puts "Número más grande en la lista: #{max(list)}"

# Invertir una cadena de caracteres
str = "Hola mundo"
puts "Cadena invertida: #{reverse_string(str)}"

# Encontrar la posición de un elemento en una lista
list = [1, 3, 5, 7, 9]
element = 5
puts "Posición del elemento #{element} en la lista: #{find_index(list, element)}"

# Ordenar una lista de números en orden ascendente
list = [5, 3, 1, 9, 7]
puts "Lista ordenada: #{sort_numbers(list)}"

# Crear un árbol binario de búsqueda
bst = BinarySearchTree.new
bst.insert(5)
bst.insert(3)
bst.insert(7)
bst.insert(1)
bst.insert(9)
puts "Árbol binario de búsqueda:"
puts bst.root.value
puts bst.root.left.value
puts bst.root.left.left.value
puts bst.root.left.right.value
puts bst.root.right.value
puts bst.root.right.left.value
puts bst.root.right.right.value

# Encontrar un elemento en el árbol binario de búsqueda
element = 7
node = bst.find(element)
puts "Elemento #{element} encontrado en el árbol binario de búsqueda: #{node.value}"
```

Explicación del código:

* La función `factorial` calcula el factorial de un número usando recursión.
* La función `max` encuentra el número más grande en una lista.
* La función `reverse_string` invierte una cadena de caracteres.
* La función `find_index` encuentra la posición de un elemento en una lista.
* La función `sort_numbers` ordena una lista de números en orden ascendente.
* La clase `Node` representa un nodo en un árbol binario de búsqueda.
* La clase `BinarySearchTree` representa un árbol binario de búsqueda.
* El programa principal calcula el factorial de 5, encuentra el número más grande en una lista, invierte una cadena de caracteres, encuentra la posición de un elemento en una lista, ordena una lista de números y crea un árbol binario de búsqueda.