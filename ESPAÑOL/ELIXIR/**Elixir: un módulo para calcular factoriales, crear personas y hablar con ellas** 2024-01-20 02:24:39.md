```elixir
# Definición de un módulo
defmodule MiModulo do

  # Definición de una función
  def calcula_factorial(numero) do
    factorial = 1
    for n <- 1..numero, reduce: factorial, do: factorial * n
  end

  # Definición de un tipo de estructura
  defstruct [:nombre, :edad]

  # Definición de una función para crear una estructura
  def crea_persona(nombre, edad) do
    %MiModulo{nombre: nombre, edad: edad}
  end

  # Definición de un tipo de enumeración
  defenum :Colores do
    :rojo
    :verde
    :azul
  end

  # Definición de un protocolo
  defprotocol Conversable do
    def habla(implementacion)
  end

  # Implementación del protocolo para el tipo de estructura `MiModulo`
  defimpl Conversable, for: MiModulo do
    def habla(persona) do
      IO.puts "Hola, mi nombre es #{persona.nombre} y tengo #{persona.edad} años."
    end
  end
end

# Uso del módulo creado
persona = MiModulo.crea_persona("Juan", 25)
MiModulo.calcula_factorial(5) |> IO.puts
MiModulo.habla(persona)
```

Explicación del código:

* Se define un módulo llamado `MiModulo`.
* Se define una función llamada `calcula_factorial` que calcula el factorial de un número.
* Se define un tipo de estructura llamado `MiModulo` que puede contener un nombre y una edad.
* Se define una función llamada `crea_persona` que crea una estructura `MiModulo` a partir de un nombre y una edad.
* Se define un tipo de enumeración llamado `Colores` que contiene los colores rojo, verde y azul.
* Se define un protocolo llamado `Conversable` que tiene una función llamada `habla`.
* Se implementa el protocolo `Conversable` para el tipo de estructura `MiModulo`.
* Se crea una estructura `MiModulo` llamada `persona` con el nombre "Juan" y la edad 25.
* Se calcula el factorial de 5 usando la función `calcula_factorial` y se imprime el resultado.
* Se llama a la función `habla` de la estructura `persona` para que la persona "hable".