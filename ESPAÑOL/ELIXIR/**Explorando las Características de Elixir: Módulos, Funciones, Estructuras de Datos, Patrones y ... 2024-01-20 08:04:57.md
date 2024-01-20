```elixir
# Definición del Módulo
defmodule HolaMundo do
  # Definición de una Función
  def saludar(a_quien) do
    IO.puts "Hola #{a_quien}!" # Imprime un Mensaje en la Consola
  end

  # Definición de una Estructura de Datos (Tuple)
  def nombre_y_apellido do
    {"Juan", "García"} # Retorna una Tupla con dos Elementos
  end

  # Definición de una Lista
  def lista_compras do
    ["Manzanas", "Naranjas", "Peras", "Platanos"] # Retorna una Lista con Cuatro Elementos
  end

  # Definición de un Mapa (Diccionario)
  def datos_personales do
    %{nombre: "Juan", apellido: "García", edad: 30} # Retorna un Mapa con Tres Claves y sus Valores Asociados
  end

  # Definición de un Patrón (Match)
  def es_mayor_de_edad?(edad) do
    case edad do
      n when n > 18 -> true # Si la Edad es Mayor a 18, Devuelve `true`
      _ -> false # Si no, Devuelve `false`
    end
  end
end

# Invocación del Módulo
HolaMundo.saludar("Mundo") # Imprime "Hola Mundo!" en la Consola
HolaMundo.nombre_y_apellido() |> IO.inspect # Imprime la Tupla en la Consola
HolaMundo.lista_compras() |> Enum.each(&IO.puts/1) # Imprime cada Elemento de la Lista en la Consola
HolaMundo.datos_personales() |> IO.inspect # Imprime el Mapa en la Consola
IO.puts HolaMundo.es_mayor_de_edad?(20) # Imprime `true` en la Consola
IO.puts HolaMundo.es_mayor_de_edad?(15) # Imprime `false` en la Consola
```

Este código es más complejo porque:

* Utiliza varias características de Elixir, como módulos, funciones, estructuras de datos, patrones y más.
* Es más largo y tiene más líneas de código.
* Es más difícil de entender y seguir.
* Es más probable que tenga errores.

Sin embargo, este código también es más poderoso y versátil. Puede usarse para resolver una variedad más amplia de problemas y puede ser más eficiente y escalable que el código simple.