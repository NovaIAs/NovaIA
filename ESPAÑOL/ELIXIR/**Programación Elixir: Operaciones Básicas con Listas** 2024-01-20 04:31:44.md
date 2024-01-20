```elixir

# Módulo principal
defmodule MiModuloPrincipal do
  # Función principal
  def main(args) do
    # Imprime un mensaje de bienvenida
    IO.puts("¡Hola, mundo!")

    # Crea una lista de números
    lista = [1, 2, 3, 4, 5]

    # Imprime la lista
    IO.inspect(lista)

    # Filtra los números pares de la lista
    lista_pares = Enum.filter(lista, &(&1 rem 2 == 0))

    # Imprime la lista de números pares
    IO.inspect(lista_pares)

    # Mapea cada número de la lista a su cuadrado
    lista_cuadrados = Enum.map(lista, &(&1 * &1))

    # Imprime la lista de cuadrados
    IO.inspect(lista_cuadrados)

    # Reduce la lista de cuadrados a su suma
    suma_cuadrados = Enum.reduce(lista_cuadrados, 0, &(&1 + &2))

    # Imprime la suma de los cuadrados
    IO.puts("La suma de los cuadrados es #{suma_cuadrados}")
  end
end

# Ejecuta el módulo principal
MiModuloPrincipal.main([])

```

Explicación del código:

* El módulo `MiModuloPrincipal` define la función `main`, que es la función principal del programa.
* La función `main` imprime un mensaje de bienvenida, crea una lista de números, filtra los números pares de la lista, mapea cada número de la lista a su cuadrado, reduce la lista de cuadrados a su suma, e imprime la suma de los cuadrados.
* La función `Enum.filter` filtra los elementos de una lista según una condición. En este caso, la condición es que el elemento sea par.
* La función `Enum.map` mapea cada elemento de una lista a un nuevo elemento. En este caso, el nuevo elemento es el cuadrado del elemento antiguo.
* La función `Enum.reduce` reduce una lista a un único valor. En este caso, el valor es la suma de los cuadrados de los elementos de la lista.
* La función `IO.puts` imprime un mensaje en la consola.
* La función `IO.inspect` imprime una representación del objeto en la consola.