```elixir
# Definición del módulo "MiModulo"
defmodule MiModulo do

  # Función "saludar/1" que recibe un nombre y devuelve un saludo
  def saludar(nombre) do
    IO.puts "Hola, #{nombre}!"
  end

  # Función "sumar/2" que recibe dos números y devuelve su suma
  def sumar(a, b) do
    a + b
  end

  # Función "es_mayor_de_edad/1" que recibe una edad y devuelve `true` si es mayor de edad (>= 18)
  def es_mayor_de_edad(edad) do
    edad >= 18
  end

  # Función recursiva "factorial/1" que recibe un número y devuelve su factorial
  def factorial(n) when n <= 1 do
    1
  end

  def factorial(n) do
    n * factorial(n - 1)
  end

  # Definición de un tipo de dato "Persona"
  deftype persona do
    @moduledoc """
    Tipo de dato "Persona" que representa a una persona con un nombre, edad y género.
    """

    # Campos del tipo de dato
    field :nombre, :string
    field :edad, :integer
    field :genero, :atom

    # Métodos del tipo de dato
    def crear(nombre, edad, genero) do
      %persona{nombre: nombre, edad: edad, genero: genero}
    end

    def nombre(persona), do: persona.nombre
    def edad(persona), do: persona.edad
    def genero(persona), do: persona.genero
  end

  # Definición de un protocolo "SerVivo" que define una función "respirar/1"
  defprotocol SerVivo do
    def respirar(ser_vivo), do: raise "No implementado"
  end

  # Implementación del protocolo "SerVivo" para el tipo de dato "Persona"
  defimpl SerVivo, for: MiModulo.persona do
    def respirar(persona) do
      IO.puts "#{persona.nombre} está respirando"
    end
  end
end

# Creación de una persona
mi_persona = MiModulo.persona.crear("Juan", 25, :masculino)

# Llamar a los métodos de la persona
MiModulo.saludar(mi_persona.nombre)
IO.puts "Edad: #{MiModulo.edad(mi_persona)}"
IO.puts "Género: #{MiModulo.genero(mi_persona)}"

# Llamar a la función "respirar/1" para la persona
MiModulo.respirar(mi_persona)

# Sumar dos números
resultado_suma = MiModulo.sumar(5, 10)
IO.puts "Suma: #{resultado_suma}"

# Calcular el factorial de un número
resultado_factorial = MiModulo.factorial(5)
IO.puts "Factorial: #{resultado_factorial}"

# Comprobar si una persona es mayor de edad
es_mayor = MiModulo.es_mayor_de_edad(mi_persona.edad)
IO.puts "Es mayor de edad: #{es_mayor}"
```

Explicación del código:

1. **Módulo "MiModulo"**: Se define un módulo llamado "MiModulo" que contendrá las funciones y tipos de dato que vamos a utilizar.

2. **Función "saludar/1"**: Esta función recibe un nombre como argumento y devuelve un saludo usando la función `IO.puts/1`.

3. **Función "sumar/2"**: Esta función recibe dos números como argumentos y devuelve su suma.

4. **Función "es_mayor_de_edad/1"**: Esta función recibe una edad como argumento y devuelve `true` si la edad es mayor o igual a 18.

5. **Función "factorial/1"**: Esta función recibe un número como argumento y calcula su factorial de forma recursiva.

6. **Tipo de dato "Persona"**: Se define un tipo de dato personalizado llamado "Persona" que tiene los campos `nombre`, `edad` y `genero`. También se definen métodos para crear personas y acceder a sus campos.

7. **Protocolo "SerVivo"**: Se define un protocolo llamado "SerVivo" que define una función `respirar/1`.

8. **Implementación del protocolo "SerVivo" para el tipo de dato "Persona"**: Se implementa el protocolo "SerVivo" para el tipo de dato "Persona", proporcionando una implementación para la función `respirar/1`.

9. **Creación de una persona**: Se crea una variable llamada `mi_persona` de tipo "Persona" utilizando la función `crear/3` del módulo `MiModulo.persona`.

10. **Llamar a los métodos de la persona**: Se llaman a los métodos `nombre/1`, `edad/1` y `genero/1` de la persona para mostrar su nombre, edad y género.

11. **Llamar a la función "respirar/1" para la persona**: Se llama a la función `respirar/1` para la persona, lo que hace que se imprima el mensaje "Juan está respirando" en la consola.

12. **Sumar dos números**: Se llama a la función `sumar/2` para sumar dos números y se muestra el resultado.

13. **Calcular el factorial de un número**: Se llama a la función `factorial/1` para calcular el factorial de un número y se muestra el resultado.

14. **Comprobar si una persona es mayor de edad**: Se llama a la función `es_mayor_de_edad/1` para comprobar si la persona es mayor de edad y se muestra el resultado.