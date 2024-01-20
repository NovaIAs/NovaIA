```elixir
# Creación de un módulo para definir un tipo de datos personalizado
defmodule Persona do
  # Propiedades del tipo de datos
  defstruct [:nombre, :apellido, :edad, :ocupacion]

  # Definición de una función de validación para el tipo de datos
  def valida(persona) do
    # Comprueba si las propiedades son válidas
    if persona.nombre =~ ~r/^([A-Za-z ]+)$/ &&
       persona.apellido =~ ~r/^([A-Za-z ]+)$/ &&
       persona.edad >= 0 &&
       persona.ocupacion =~ ~r/^([A-Za-z ]+)$/ do

      # Devuelve la persona si es válida
      {:ok, persona}
    else
      # Devuelve un error si no es válida
      {:error, "Persona no válida"}
    end
  end

  # Definición de una función para comparar dos personas
  def compara(persona1, persona2) do
    # Comprueba si las dos personas son iguales
    if persona1.nombre == persona2.nombre &&
       persona1.apellido == persona2.apellido &&
       persona1.edad == persona2.edad &&
       persona1.ocupacion == persona2.ocupacion do

      # Devuelve true si las dos personas son iguales
      true
    else
      # Devuelve false si las dos personas no son iguales
      false
    end
  end

  # Definición de una función para imprimir una persona
  def imprime(persona) do
    # Imprime las propiedades de la persona
    IO.puts "Nombre: #{persona.nombre}"
    IO.puts "Apellido: #{persona.apellido}"
    IO.puts "Edad: #{persona.edad}"
    IO.puts "Ocupación: #{persona.ocupacion}"
  end
end

# Creación de una persona
persona1 = %Persona{nombre: "Juan", apellido: "Pérez", edad: 25, ocupacion: "Ingeniero"}

# Impresión de la persona
Persona.imprime(persona1)

# Obteniendo el tipo de datos de la persona
IO.puts "Tipo de datos de la persona: #{inspect persona1}"

# Validación de la persona
{:resultado, persona_validada} = Persona.valida(persona1)
if resultado == :ok do
  IO.puts "Persona válida: #{inspect persona_validada}"
else
  IO.puts "Persona no válida: #{inspect persona1}"
end

# Comparación de dos personas
persona2 = %Persona{nombre: "María", apellido: "Martínez", edad: 28, ocupacion: "Médica"}
if Persona.compara(persona1, persona2) do
  IO.puts "Las dos personas son iguales"
else
  IO.puts "Las dos personas no son iguales"
end
```

Explicación del código:

1. Se crea un módulo `Persona` que define un tipo de datos personalizado para representar información sobre personas.
2. El tipo de datos `Persona` tiene cuatro propiedades: `nombre`, `apellido`, `edad` y `ocupacion`.
3. Se define una función de validación `Persona.valida/1` que comprueba si las propiedades de una persona son válidas.
4. Se define una función de comparación `Persona.compara/2` que compara dos personas y devuelve `true` si son iguales y `false` si son diferentes.
5. Se define una función de impresión `Persona.imprime/1` que imprime las propiedades de una persona.
6. Se crea una instancia de `Persona` con los datos de una persona.
7. Se imprime la persona utilizando la función `Persona.imprime/1`.
8. Se obtiene el tipo de datos de la persona utilizando la función `inspect/1`.
9. Se valida la persona utilizando la función `Persona.valida/1`.
10. Se comparan dos personas utilizando la función `Persona.compara/2`.