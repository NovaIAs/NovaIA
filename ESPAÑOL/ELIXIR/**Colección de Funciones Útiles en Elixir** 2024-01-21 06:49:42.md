```elixir
defmodule Calculadora do
  def sumar(a, b) do
    a + b
  end

  def restar(a, b) do
    a - b
  end

  def multiplicar(a, b) do
    a * b
  end

  def dividir(a, b) do
    if b == 0, do: {:error, "No se puede dividir por cero"}, else: a / b
  end

  def potencia(a, b) do
    :math.pow(a, b)
  end

  def factorial(n) do
    if n == 0, do: 1, else: n * factorial(n-1)
  end
end

defmodule UnidadDistancia do
  def kilometro(n) do
    n * 1000
  end

  def metro(n) do
    n
  end

  def centimetro(n) do
    n / 100
  end

  def milla(n) do
    n * 1609.34
  end

  def yarda(n) do
    n * 0.9144
  end

  def pie(n) do
    n * 0.3048
  end

  def pulgada(n) do
    n * 0.0254
  end
end

defmodule Persona do
  defstruct [:nombre, :edad, :estatura, :peso]
end

defmodule Banco do
  def cuenta_ahorro(n) do
    %{numero: n, saldo: 0}
  end

  def depositar(cuenta, monto) do
    cuenta.saldo += monto
    cuenta
  end

  def retirar(cuenta, monto) do
    if cuenta.saldo >= monto do
      cuenta.saldo -= monto
      cuenta
    else
      {:error, "Saldo insuficiente"}
    end
  end
end

defmodule Lista do
  def crear() do
    []
  end

  def agregar(lista, elemento) do
    [elemento | lista]
  end

  def eliminar(lista, elemento) do
    Enum.filter(lista, fn x -> x != elemento end)
  end

  def primero(lista) do
    hd(lista)
  end

  def ultimo(lista) do
    tl(lista) |> ultimo()
  end
end

defmodule ArbolBinario do
  def crear() do
    nil
  end

  def insertar(arbol, valor) do
    cond do
      is_nil(arbol) -> {valor, crear(), crear()}
      valor < arbol -> {arbol, insertar(arbol.izquierda, valor), arbol.derecha}
      valor > arbol -> {arbol, arbol.izquierda, insertar(arbol.derecha, valor)}
    end
  end

  def buscar(arbol, valor) do
    cond do
      is_nil(arbol) -> false
      valor == arbol -> true
      valor < arbol -> buscar(arbol.izquierda, valor)
      valor > arbol -> buscar(arbol.derecha, valor)
    end
  end

  def eliminar(arbol, valor) do
    cond do
      is_nil(arbol) -> nil
      valor < arbol -> {arbol, eliminar(arbol.izquierda, valor), arbol.derecha}
      valor > arbol -> {arbol, arbol.izquierda, eliminar(arbol.derecha, valor)}
      true -> eliminar_nodo(arbol)
    end
  end

  defp eliminar_nodo(nodo) do
    cond do
      is_nil(nodo.izquierda) and is_nil(nodo.derecha) -> nil
      is_nil(nodo.izquierda) -> {nodo.derecha, crear(), crear()}
      is_nil(nodo.derecha) -> {nodo.izquierda, crear(), crear()}
      true -> {nodo.derecha, crear(), crear()}
    end
  end
end

defmodule Pila do
  def crear() do
    []
  end

  def apilar(pila, elemento) do
    [elemento | pila]
  end

  def desapilar(pila) do
    tl(pila)
  end

  def tope(pila) do
    hd(pila)
  end

  def esta_vacia?(pila) do
    is_nil(pila) or pila == []
  end
end

defmodule Cola do
  def crear() do
    []
  end

  def encolar(cola, elemento) do
    cola ++ [elemento]
  end

  def desencolar(cola) do
    tl(cola)
  end

  def frente(cola) do
    hd(cola)
  end

  def esta_vacia?(cola) do
    is_nil(cola) or cola == []
  end
end
```

Explicación del código:

* El módulo `Calculadora` define las funciones básicas de una calculadora, como sumar, restar, multiplicar, dividir, calcular la potencia y el factorial.
* El módulo `UnidadDistancia` define las funciones para convertir entre diferentes unidades de distancia.
* El módulo `Persona` define la estructura de datos para representar a una persona, con atributos como nombre, edad, estatura y peso.
* El módulo `Banco` define las funciones para crear, depositar y retirar dinero de una cuenta de ahorro.
* El módulo `Lista` define las funciones para crear, agregar, eliminar, obtener el primer y último elemento de una lista.
* El módulo `ArbolBinario` define las funciones para crear, insertar, buscar y eliminar elementos de un árbol binario.
* El módulo `Pila` define las funciones para crear, apilar, desapilar y obtener el tope de una pila.
* El módulo `Cola` define las funciones para crear, encolar, desencolar y obtener el frente de una cola.