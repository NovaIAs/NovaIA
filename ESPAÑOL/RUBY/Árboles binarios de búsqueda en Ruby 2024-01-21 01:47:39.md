```ruby
# Clase para representar un árbol binario de búsqueda
class ArbolBinarioDeBusqueda

  # Nodo para representar un elemento del árbol
  class Nodo
    attr_accessor :valor, :izquierda, :derecha

    def initialize(valor)
      @valor = valor
      @izquierda = nil
      @derecha = nil
    end
  end

  # Raíz del árbol
  attr_accessor :raiz

  # Constructor
  def initialize
    @raiz = nil
  end

  # Insertar un elemento en el árbol
  def insertar(valor)
    nuevo_nodo = Nodo.new(valor)
    if @raiz.nil?
      @raiz = nuevo_nodo
    else
      insertar_recursivo(nuevo_nodo, @raiz)
    end
  end

  # Insertar un elemento en el árbol de forma recursiva
  def insertar_recursivo(nuevo_nodo, nodo_actual)
    if nuevo_nodo.valor < nodo_actual.valor
      if nodo_actual.izquierda.nil?
        nodo_actual.izquierda = nuevo_nodo
      else
        insertar_recursivo(nuevo_nodo, nodo_actual.izquierda)
      end
    else
      if nodo_actual.derecha.nil?
        nodo_actual.derecha = nuevo_nodo
      else
        insertar_recursivo(nuevo_nodo, nodo_actual.derecha)
      end
    end
  end

  # Buscar un elemento en el árbol
  def buscar(valor)
    buscar_recursivo(valor, @raiz)
  end

  # Buscar un elemento en el árbol de forma recursiva
  def buscar_recursivo(valor, nodo_actual)
    if nodo_actual.nil?
      return nil
    elsif nodo_actual.valor == valor
      return nodo_actual
    elsif valor < nodo_actual.valor
      return buscar_recursivo(valor, nodo_actual.izquierda)
    else
      return buscar_recursivo(valor, nodo_actual.derecha)
    end
  end

  # Eliminar un elemento del árbol
  def eliminar(valor)
    nodo_a_eliminar = buscar(valor)
    if nodo_a_eliminar.nil?
      return nil
    end
    if nodo_a_eliminar.izquierda.nil? && nodo_a_eliminar.derecha.nil?
      eliminar_hoja(nodo_a_eliminar)
    elsif nodo_a_eliminar.izquierda.nil? || nodo_a_eliminar.derecha.nil?
      eliminar_nodo_con_un_hijo(nodo_a_eliminar)
    else
      eliminar_nodo_con_dos_hijos(nodo_a_eliminar)
    end
  end

  # Eliminar una hoja del árbol
  def eliminar_hoja(nodo_a_eliminar)
    nodo_padre = nodo_a_eliminar.padre
    if nodo_padre.nil?
      @raiz = nil
    elsif nodo_padre.izquierda == nodo_a_eliminar
      nodo_padre.izquierda = nil
    else
      nodo_padre.derecha = nil
    end
  end

  # Eliminar un nodo con un solo hijo
  def eliminar_nodo_con_un_hijo(nodo_a_eliminar)
    nodo_padre = nodo_a_eliminar.padre
    nodo_hijo = nodo_a_eliminar.izquierda.nil? ? nodo_a_eliminar.derecha : nodo_a_eliminar.izquierda
    if nodo_padre.nil?
      @raiz = nodo_hijo
    elsif nodo_padre.izquierda == nodo_a_eliminar
      nodo_padre.izquierda = nodo_hijo
    else
      nodo_padre.derecha = nodo_hijo
    end
  end

  # Eliminar un nodo con dos hijos
  def eliminar_nodo_con_dos_hijos(nodo_a_eliminar)
    nodo_predecesor = nodo_a_eliminar.predecesor
    nodo_a_eliminar.valor = nodo_predecesor.valor
    eliminar_hoja(nodo_predecesor)
  end

  # Obtener el nodo predecesor de un nodo dado
  def predecesor(nodo)
    if nodo.izquierda.nil?
      return nil
    end
    nodo_actual = nodo.izquierda
    while nodo_actual.derecha.nil?
      nodo_actual = nodo_actual.derecha
    end
    return nodo_actual
  end

  # Obtener el nodo sucesor de un nodo dado
  def sucesor(nodo)
    if nodo.derecha.nil?
      return nil
    end
    nodo_actual = nodo.derecha
    while nodo_actual.izquierda.nil?
      nodo_actual = nodo_actual.izquierda
    end
    return nodo_actual
  end

  # Recorrer el árbol en orden ascendente
  def inorden(nodo_actual = @raiz)
    if nodo_actual.nil?
      return
    end
    inorden(nodo_actual.izquierda)
    puts nodo_actual.valor
    inorden(nodo_actual.derecha)
  end

  # Recorrer el árbol en orden descendente
  def postorden(nodo_actual = @raiz)
    if nodo_actual.nil?
      return
    end
    postorden(nodo_actual.izquierda)
    postorden(nodo_actual.derecha)
    puts nodo_actual.valor
  end

  # Recorrer el árbol en orden ancho
  def anchura(nodo_actual = @raiz)
    cola = [nodo_actual]
    while !cola.empty?
      nodo_actual = cola.shift
      puts nodo_actual.valor
      cola.push(nodo_actual.izquierda) unless nodo_actual.izquierda.nil?
      cola.push(nodo_actual.derecha) unless nodo_actual.derecha.nil?
    end
  end
end

# Ejemplo de uso
arbol = ArbolBinarioDeBusqueda.new()
arbol.insertar(10)
arbol.insertar(5)
arbol.insertar(15)
arbol.insertar(2)
arbol.insertar(7)
arbol.insertar(12)
arbol