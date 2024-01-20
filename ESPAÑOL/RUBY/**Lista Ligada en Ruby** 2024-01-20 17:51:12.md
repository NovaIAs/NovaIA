```ruby
# Clase para crear una lista ligada
class ListaLigada
  # Clase para crear un nodo
  class Nodo
    attr_accessor :dato, :siguiente

    def initialize(dato)
      @dato = dato
      @siguiente = nil
    end
  end

  attr_accessor :cabeza, :cola

  def initialize
    @cabeza = nil
    @cola = nil
  end

  # Método para insertar un nuevo nodo al final de la lista
  def insertar(dato)
    nuevo_nodo = Nodo.new(dato)
    if @cabeza.nil?
      @cabeza = nuevo_nodo
      @cola = nuevo_nodo
    else
      @cola.siguiente = nuevo_nodo
      @cola = nuevo_nodo
    end
  end

  # Método para eliminar un nodo por su dato
  def eliminar(dato)
    actual = @cabeza
    anterior = nil
    while actual != nil && actual.dato != dato
      anterior = actual
      actual = actual.siguiente
    end

    if actual != nil
      if anterior.nil?
        @cabeza = actual.siguiente
      else
        anterior.siguiente = actual.siguiente
      end

      if actual == @cola
        @cola = anterior
      end
    end
  end

  # Método para buscar un nodo por su dato
  def buscar(dato)
    actual = @cabeza
    while actual != nil && actual.dato != dato
      actual = actual.siguiente
    end

    return actual
  end

  # Método para imprimir la lista
  def imprimir
    actual = @cabeza
    while actual != nil
      puts actual.dato
      actual = actual.siguiente
    end
  end
end

# Crea una nueva lista ligada
lista = ListaLigada.new

# Inserta 5 elementos a la lista
lista.insertar(10)
lista.insertar(20)
lista.insertar(30)
lista.insertar(40)
lista.insertar(50)

# Imprime la lista
puts "Lista original:"
lista.imprimir

# Elimina el elemento con dato 30
lista.eliminar(30)

# Imprime la lista después de eliminar el elemento
puts "Lista después de eliminar el elemento 30:"
lista.imprimir

# Busca el elemento con dato 40
elemento = lista.buscar(40)

# Imprime el elemento encontrado
puts "Elemento encontrado con dato 40:"
puts elemento.dato
```

Explicación del código:

* La clase `ListaLigada` representa una lista ligada.
* La clase `Nodo` representa un nodo de la lista.
* El método `insertar` inserta un nuevo nodo al final de la lista.
* El método `eliminar` elimina un nodo de la lista por su dato.
* El método `buscar` busca un nodo en la lista por su dato.
* El método `imprimir` imprime la lista.

El código crea una nueva lista ligada e inserta 5 elementos a la lista. Luego, elimina el elemento con dato 30 e imprime la lista después de eliminar el elemento. Finalmente, busca el elemento con dato 40 e imprime el elemento encontrado.