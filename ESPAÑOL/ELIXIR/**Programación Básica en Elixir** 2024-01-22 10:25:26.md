```elixir
defmodule HolaMundo do
  def hello(nombre) do
    "Hola, #{nombre}!"
  end
end

defmodule Suma do
  def suma(a, b) do
    a + b
  end
end

defmodule Resta do
  def resta(a, b) do
    a - b
  end
end

defmodule Multiplicacion do
  def multiplicacion(a, b) do
    a * b
  end
end

defmodule Division do
  def division(a, b) do
    a / b
  end
end

defmodule Potencia do
  def potencia(a, b) do
    a ** b
  end
end

defmodule RaizCuadrada do
  def raiz_cuadrada(a) do
    :math.sqrt(a)
  end
end

defmodule Factorial do
  def factorial(a) do
    if a == 0 do
      1
    else
      a * factorial(a - 1)
    end
  end
end

defmodule Fibonacci do
  def fibonacci(n) do
    if n == 0 do
      0
    elsif n == 1 do
      1
    else
      fibonacci(n - 1) + fibonacci(n - 2)
    end
  end
end

defmodule Primo do
  def primo?(n) when n < 2 do
    false
  end

  def primo?(n) do
    if n == 2 do
      true
    else
      primo?(n, 2)
    end
  end

  defp primo?(n, i) when i * i > n do
    true
  end

  defp primo?(n, i) do
    if rem(n, i) == 0 do
      false
    else
      primo?(n, i + 1)
    end
  end
end

defmodule Palindromo do
  def palindromo?(palabra) do
    palabra == :string.reverse(palabra)
  end
end

defmodule Anagrama do
  def anagrama?(palabra1, palabra2) do
    :string.sort(palabra1) == :string.sort(palabra2)
  end
end

defmodule OrdenarLista do
  def ordenar_lista(lista) do
    :lists.sort(lista)
  end
end

defmodule InvertirLista do
  def invertir_lista(lista) do
    :lists.reverse(lista)
  end
end

defmodule ConcatenarLista do
  def concatenar_lista(lista1, lista2) do
    lista1 ++ lista2
  end
end

defmodule FiltrarLista do
  def filtrar_lista(lista, condicion) do
    :lists.filter(condicion, lista)
  end
end

defmodule MapearLista do
  def mapear_lista(lista, funcion) do
    :lists.map(funcion, lista)
  end
end

defmodule ReducirLista do
  def reducir_lista(lista, funcion, acumulador) do
    :lists.reduce(funcion, acumulador, lista)
  end
end

defmodule BuscarElementoLista do
  def buscar_elemento_lista(lista, elemento) do
    :lists.member(elemento, lista)
  end
end

defmodule EncontrarIndiceElementoLista do
  def encontrar_indice_elemento_lista(lista, elemento) do
    :lists.index_of(elemento, lista)
  end
end

defmodule EliminarElementoLista do
  def eliminar_elemento_lista(lista, elemento) do
    :lists.delete(elemento, lista)
  end
end

defmodule InsertarElementoLista do
  def insertar_elemento_lista(lista, elemento, posicion) do
    :lists.insert_at(lista, posicion, elemento)
  end
end

defmodule RotarLista do
  def rotar_lista(lista, n) do
    :lists.rot(n, lista)
  end
end

defmodule ParticionarLista do
  def particionar_lista(lista, n) do
    :lists.partition(n, lista)
  end
end

defmodule AgruparLista do
  def agrupar_lista(lista, n