```elixir
# Módulo principal
defmodule MiModuloPrincipal do

  # Función principal
  def main() do

    # Crear una lista de números
    numeros = [1, 2, 3, 4, 5]

    # Mapear la lista de números a una lista de sus cuadrados
    cuadrados = Enum.map(numeros, fn x -> x * x end)

    # Filtrar la lista de cuadrados para obtener sólo los que sean mayores que 10
    cuadrados_mayores_que_10 = Enum.filter(cuadrados, fn x -> x > 10 end)

    # Imprimir la lista de cuadrados mayores que 10
    IO.puts("Cuadrados mayores que 10:")
    Enum.each(cuadrados_mayores_que_10, fn x -> IO.puts(x) end)
  end
end

# Iniciar el módulo principal
MiModuloPrincipal.main()
```

Explicación:

1. Primero, creamos un módulo llamado `MiModuloPrincipal`. Un módulo es una unidad de encapsulamiento en Elixir. Contiene funciones, estructuras de datos y otras definiciones.

2. A continuación, definimos una función llamada `main`, que es la función principal de nuestro programa. La función `main` se ejecuta cuando se inicia el programa.

3. En la función `main`, creamos una lista de números del 1 al 5 utilizando la lista literal `[1, 2, 3, 4, 5]`.

4. A continuación, utilizamos la función `Enum.map` para mapear la lista de números a una lista de sus cuadrados. La función `Enum.map` aplica una función a cada elemento de una lista y devuelve una nueva lista con los resultados. En este caso, la función que estamos aplicando es `fn x -> x * x end`, que simplemente multiplica cada número por sí mismo.

5. A continuación, utilizamos la función `Enum.filter` para filtrar la lista de cuadrados y obtener sólo los que sean mayores que 10. La función `Enum.filter` devuelve una nueva lista con los elementos que satisfacen una condición determinada. En este caso, la condición es `x > 10`, que comprueba si el número es mayor que 10.

6. Por último, utilizamos la función `IO.puts` para imprimir la lista de cuadrados mayores que 10. La función `IO.puts` imprime una línea de texto en la consola.

Cuando se ejecuta este programa, producirá la siguiente salida:

```
Cuadrados mayores que 10:
16
25
```