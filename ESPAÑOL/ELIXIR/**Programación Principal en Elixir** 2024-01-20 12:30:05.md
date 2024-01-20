```elixir
# Módulo principal
defmodule MiModuloPrincipal do
  def main() do
    # Crea una lista de números
    numeros = [1, 2, 3]

    # Iterar sobre la lista con un bucle for
    for numero <- numeros do
      # Imprime el número actual
      IO.puts "Número actual: #{numero}"
    end

    # Utiliza una función anónima para filtrar la lista
    numeros_pares = Enum.filter(numeros, fn numero -> rem(numero, 2) == 0 end)

    # Imprime los números pares
    IO.puts "Números pares: #{numeros_pares}"

    # Crea un mapa con claves y valores
    persona = %{nombre: "Juan", apellido: "García", edad: 30}

    # Accede a los valores del mapa usando la sintaxis de punto
    IO.puts "Nombre: #{persona.nombre}"
    IO.puts "Apellido: #{persona.apellido}"
    IO.puts "Edad: #{persona.edad}"

    # Utiliza un try/catch para manejar errores
    try do
      # Intenta abrir un archivo que no existe
      File.open("archivo_inexistente.txt", [:read])
    catch
      :error, reason ->
        # Captura el error y lo imprime
        IO.puts "Se ha producido un error: #{inspect reason}"
    end
  end
end

# Llama a la función principal
MiModuloPrincipal.main()
```

Explicación del código:

* El código comienza definiendo un módulo llamado `MiModuloPrincipal`.
* Dentro del módulo, se define una función llamada `main`, que es la función principal del programa.
* La función `main` crea una lista de números y luego imprime los números pares de la lista.
* También crea un mapa con claves y valores, y luego accede a los valores del mapa usando la sintaxis de punto.
* Por último, la función `main` utiliza un try/catch para manejar errores. Intenta abrir un archivo que no existe, y si se produce un error, lo captura y lo imprime.