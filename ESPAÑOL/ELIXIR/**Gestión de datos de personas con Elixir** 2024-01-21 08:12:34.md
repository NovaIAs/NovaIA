```elixir
# Definimos un módulo que contendrá nuestras funciones y estructuras de datos.
defmodule MiModulo do

  # Definimos una estructura de datos llamada "Persona".
  defstruct Persona do
    nombre
    edad
  end

  # Definimos una función que crea una nueva persona.
  def crear_persona(nombre, edad) do
    %Persona{
      nombre: nombre,
      edad: edad
    }
  end

  # Definimos una función que devuelve una lista de personas.
  def lista_personas() do
    [
      crear_persona("Juan", 20),
      crear_persona("María", 25),
      crear_persona("Pedro", 30)
    ]
  end

  # Definimos una función que devuelve la edad media de una lista de personas.
  def edad_media(lista_personas) do
    Enum.reduce(lista_personas, 0, fn persona, edad_media ->
      edad_media + persona.edad
    end) / Enum.count(lista_personas)
  end

  # Definimos una función que devuelve la persona más joven de una lista de personas.
  def persona_mas_joven(lista_personas) do
    Enum.reduce(lista_personas, nil, fn persona, persona_mas_joven ->
      if persona.edad < persona_mas_joven.edad do
        persona
      else
        persona_mas_joven
      end
    end)
  end
end

# Llamamos a la función "lista_personas" para obtener una lista de personas.
lista_personas = MiModulo.lista_personas()

# Llamamos a la función "edad_media" para obtener la edad media de la lista de personas.
edad_media = MiModulo.edad_media(lista_personas)

# Mostramos la edad media por consola.
IO.puts("La edad media es: #{edad_media}")

# Llamamos a la función "persona_mas_joven" para obtener la persona más joven de la lista de personas.
persona_mas_joven = MiModulo.persona_mas_joven(lista_personas)

# Mostramos el nombre de la persona más joven por consola.
IO.puts("La persona más joven es: #{persona_mas_joven.nombre}")
```

Explicación del código:

* El módulo `MiModulo` define las funciones y estructuras de datos que utilizaremos en nuestro programa.
* La estructura de datos `Persona` define un tipo de datos que representa a una persona. Tiene dos campos: `nombre` y `edad`.
* La función `crear_persona` crea una nueva persona a partir de su nombre y edad.
* La función `lista_personas` devuelve una lista de personas.
* La función `edad_media` devuelve la edad media de una lista de personas.
* La función `persona_mas_joven` devuelve la persona más joven de una lista de personas.
* En el código principal, llamamos a la función `lista_personas` para obtener una lista de personas.
* Llamamos a la función `edad_media` para obtener la edad media de la lista de personas.
* Mostramos la edad media por consola.
* Llamamos a la función `persona_mas_joven` para obtener la persona más joven de la lista de personas.
* Mostramos el nombre de la persona más joven por consola.