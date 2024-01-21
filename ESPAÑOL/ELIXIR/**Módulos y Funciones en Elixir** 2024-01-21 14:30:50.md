```elixir
defmodule HolaMundo do
  def saludar() do
    IO.puts "Hola mundo!"
  end
end

HolaMundo.saludar()

defmodule Sumador do
  def suma(a, b) do
    a + b
  end
end

Sumador.suma(1, 2) |> IO.inspect

defmodule Lista do
  def crear() do
    []
  end

  def agregar(lista, elemento) do
    [elemento | lista]
  end

  def quitar(lista, elemento) do
    lista -- [elemento]
  end

  def largo(lista) do
    length(lista)
  end
end

lista = Lista.crear()
lista = Lista.agregar(lista, 1)
lista = Lista.agregar(lista, 2)
lista = Lista.agregar(lista, 3)
Lista.largo(lista) |> IO.inspect

defmodule Mapa do
  def crear() do
    %{}
  end

  def poner(mapa, clave, valor) do
    Map.put(mapa, clave, valor)
  end

  def quitar(mapa, clave) do
    Map.delete(mapa, clave)
  end

  def obtener(mapa, clave) do
    Map.get(mapa, clave)
  end

  def largo(mapa) do
    length(mapa)
  end
end

mapa = Mapa.crear()
mapa = Mapa.poner(mapa, :nombre, "Juan")
mapa = Mapa.poner(mapa, :edad, 20)
Mapa.obtener(mapa, :nombre) |> IO.inspect

defmodule Punto do
  def crear(x, y) do
    %Punto{x: x, y: y}
  end

  def x(punto) do
    punto.x
  end

  def y(punto) do
    punto.y
  end

  def mover(punto, dx, dy) do
    %Punto{punto | x: punto.x + dx, y: punto.y + dy}
  end

  def distancia(punto1, punto2) do
    :math.sqrt((punto2.x - punto1.x)^2 + (punto2.y - punto1.y)^2)
  end
end

punto1 = Punto.crear(1, 2)
punto2 = Punto.crear(3, 4)
Punto.distancia(punto1, punto2) |> IO.inspect
```

Este código Elixir es un ejemplo de cómo crear varios módulos y funciones en el mismo archivo. También muestra cómo usar algunas de las funciones estándar de Elixir, como `IO.puts`, `length`, `Map.put`, `Map.get`, `Map.delete` y `:math.sqrt`.

El primer módulo, `HolaMundo`, simplemente define una función `saludar` que imprime "Hola mundo!" en la consola.

El segundo módulo, `Sumador`, define una función `suma` que suma dos números.

El tercer módulo, `Lista`, define varias funciones para trabajar con listas, como `crear`, `agregar`, `quitar` y `largo`.

El cuarto módulo, `Mapa`, define varias funciones para trabajar con mapas, como `crear`, `poner`, `quitar`, `obtener` y `largo`.

El quinto módulo, `Punto`, define varias funciones para trabajar con puntos en un plano bidimensional, como `crear`, `x`, `y`, `mover` y `distancia`.

El código también incluye algunos ejemplos de cómo usar estos módulos y funciones. Por ejemplo, el código crea una lista, agrega algunos elementos a la lista, y luego imprime la longitud de la lista. También crea un mapa, pone algunos valores en el mapa, y luego obtiene un valor del mapa. Finalmente, el código crea dos puntos, calcula la distancia entre los puntos, y luego imprime la distancia.