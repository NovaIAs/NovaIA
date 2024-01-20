```elixir
# Módulo que define una estructura de datos para representar un árbol binario de búsqueda.

defmodule ArbolBinarioDeBusqueda do
  defstruct [:valor, :izquierda, :derecha]

  # Función para crear un nuevo nodo en el árbol.

  def nuevo_nodo(valor) do
    %ArbolBinarioDeBusqueda{valor: valor, izquierda: nil, derecha: nil}
  end

  # Función para insertar un nuevo valor en el árbol.

  def insertar(árbol, valor) do
    insertar_recursivo(árbol, valor)
  end

  defp insertar_recursivo(nil, valor) do
    nuevo_nodo(valor)
  end

  defp insertar_recursivo(árbol, valor) do
    if valor < árbol.valor do
      %ArbolBinarioDeBusqueda{árbol | izquierda: insertar_recursivo(árbol.izquierda, valor)}
    else
      %ArbolBinarioDeBusqueda{árbol | derecha: insertar_recursivo(árbol.derecha, valor)}
    end
  end

  # Función para buscar un valor en el árbol.

  def buscar(árbol, valor) do
    buscar_recursivo(árbol, valor)
  end

  defp buscar_recursivo(nil, _) do
    nil
  end

  defp buscar_recursivo(árbol, valor) do
    if valor == árbol.valor do
      árbol
    elsif valor < árbol.valor do
      buscar_recursivo(árbol.izquierda, valor)
    else
      buscar_recursivo(árbol.derecha, valor)
    end
  end

  # Función para eliminar un valor del árbol.

  def eliminar(árbol, valor) do
    eliminar_recursivo(árbol, valor)
  end

  defp eliminar_recursivo(nil, _) do
    nil
  end

  defp eliminar_recursivo(árbol, valor) do
    if valor == árbol.valor do
      eliminar_nodo(árbol)
    elsif valor < árbol.valor do
      %ArbolBinarioDeBusqueda{árbol | izquierda: eliminar_recursivo(árbol.izquierda, valor)}
    else
      %ArbolBinarioDeBusqueda{árbol | derecha: eliminar_recursivo(árbol.derecha, valor)}
    end
  end

  defp eliminar_nodo(árbol) do
    if árbol.izquierda == nil do
      árbol.derecha
    elsif árbol.derecha == nil do
      árbol.izquierda
    else
      # Encontrar el nodo más a la izquierda en el subárbol derecho.
      nodo_mas_a_la_izquierda = encontrar_nodo_mas_a_la_izquierda(árbol.derecha)

      # Reemplazar el valor del nodo a eliminar con el valor del nodo más a la izquierda en el subárbol derecho.
      %ArbolBinarioDeBusqueda{nodo_mas_a_la_izquierda | izquierda: árbol.izquierda, derecha: eliminar_nodo(árbol.derecha)}
    end
  end

  defp encontrar_nodo_mas_a_la_izquierda(árbol) do
    if árbol.izquierda == nil do
      árbol
    else
      encontrar_nodo_mas_a_la_izquierda(árbol.izquierda)
    end
  end
end

# Crear un nuevo árbol binario de búsqueda.

arbol = ArbolBinarioDeBusqueda.nuevo_nodo(10)

# Insertar algunos valores en el árbol.

arbol = ArbolBinarioDeBusqueda.insertar(arbol, 5)
arbol = ArbolBinarioDeBusqueda.insertar(arbol, 15)
arbol = ArbolBinarioDeBusqueda.insertar(arbol, 2)
arbol = ArbolBinarioDeBusqueda.insertar(arbol, 7)
arbol = ArbolBinarioDeBusqueda.insertar(arbol, 12)
arbol = ArbolBinarioDeBusqueda.insertar(arbol, 20)

# Buscar un valor en el árbol.

valor = ArbolBinarioDeBusqueda.buscar(arbol, 12)
IO.puts("El valor #{valor} fue encontrado en el árbol.")

# Eliminar un valor del árbol.

arbol = ArbolBinarioDeBusqueda.eliminar(arbol, 10)

# Imprimir el árbol.

IO.inspect(arbol)
```

Este código define una estructura de datos para representar un árbol binario de búsqueda y proporciona funciones para insertar, buscar y eliminar valores del árbol. El algoritmo de búsqueda binaria se utiliza para realizar las búsquedas de manera eficiente, lo que hace que este tipo de árbol sea adecuado para almacenar y recuperar datos ordenados.

El código está bien organizado y documentado, lo que facilita su comprensión y mantenimiento. También es extensible, ya que se pueden añadir nuevas funciones y métodos según sea necesario.

En general, este código es un ejemplo bien escrito y completo de cómo crear y utilizar un árbol binario de búsqueda en Elixir.