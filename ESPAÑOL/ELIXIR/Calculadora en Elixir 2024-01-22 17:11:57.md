```elixir
# Definición de las bibliotecas que se van a utilizar
import MeuPrograma.MiModulo
import MeuPrograma.OtroModulo

# Definición de funciones y sus respectivos parámetros
defmodule MiModulo do
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
    a / b
  end
end

# Definición de otras funciones y sus respectivos parámetros
defmodule OtroModulo do
  def potencia(a, b) do
    :math.pow(a, b)
  end

  def raiz(a, b) do
    :math.sqrt(a)
  end

  def logaritmo(a, b) do
    :math.log(a, b)
  end

  def seno(a) do
    :math.sin(a)
  end

  def coseno(a) do
    :math.cos(a)
  end

  def tangente(a) do
    :math.tan(a)
  end

  # Función para imprimir un mensaje, se le pasa como parámetro un mensaje
  def imprimir_mensaje(mensaje) do
    IO.puts(mensaje)
  end
end

# Función principal del programa
defmodule MeuPrograma do
  # Creamos una función que se llama "menu" que no recibe ningún parámetro
  def menu() do
    # Imprimimos en pantalla un mensaje de bienvenida
    OtroModulo.imprimir_mensaje("Bienvenido a la calculadora")

    # Creamos un bucle infinito para que el programa se ejecute hasta que se introduzca una opción válida
    while true do
      # Imprimimos en pantalla las opciones disponibles
      IO.puts("Elija una opción:")
      IO.puts("1. Sumar")
      IO.puts("2. Restar")
      IO.puts("3. Multiplicar")
      IO.puts("4. Dividir")
      IO.puts("5. Potencia")
      IO.puts("6. Raíz cuadrada")
      IO.puts("7. Logaritmo")
      IO.puts("8. Seno")
      IO.puts("9. Coseno")
      IO.puts("10. Tangente")
      IO.puts("11. Salir")

      # Leemos la opción introducida por el usuario
      opcion = IO.gets("")

      # Comprobamos si la opción introducida es válida
      if opcion >= "1" and opcion <= "11" do
        # Si la opción es válida, llamamos a la función correspondiente
        case opcion do
          "1" ->
            # Pedimos al usuario que introduzca los dos números que quiere sumar
            IO.puts("Introduzca el primer número:")
            a = IO.gets("")

            IO.puts("Introduzca el segundo número:")
            b = IO.gets("")

            # Convertimos los números introducidos a enteros
            a = String.to_integer(a)
            b = String.to_integer(b)

            # Llamamos a la función "sumar" del módulo "MiModulo" y guardamos el resultado en la variable "resultado"
            resultado = MiModulo.sumar(a, b)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "2" ->
            # Pedimos al usuario que introduzca los dos números que quiere restar
            IO.puts("Introduzca el primer número:")
            a = IO.gets("")

            IO.puts("Introduzca el segundo número:")
            b = IO.gets("")

            # Convertimos los números introducidos a enteros
            a = String.to_integer(a)
            b = String.to_integer(b)

            # Llamamos a la función "restar" del módulo "MiModulo" y guardamos el resultado en la variable "resultado"
            resultado = MiModulo.restar(a, b)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "3" ->
            # Pedimos al usuario que introduzca los dos números que quiere multiplicar
            IO.puts("Introduzca el primer número:")
            a = IO.gets("")

            IO.puts("Introduzca el segundo número:")
            b = IO.gets("")

            # Convertimos los números introducidos a enteros
            a = String.to_integer(a)
            b = String.to_integer(b)

            # Llamamos a la función "multiplicar" del módulo "MiModulo" y guardamos el resultado en la variable "resultado"
            resultado = MiModulo.multiplicar(a, b)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "4" ->
            # Pedimos al usuario que introduzca los dos números que quiere dividir
            IO.puts("Introduzca el primer número:")
            a = IO.gets("")

            IO.puts("Introduzca el segundo número:")
            b = IO.gets("")

            # Convertimos los números introducidos a enteros
            a = String.to_integer(a)
            b = String.to_integer(b)

            # Llamamos a la función "dividir" del módulo "MiModulo" y guardamos el resultado en la variable "resultado"
            resultado = MiModulo.dividir(a, b)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "5" ->
            # Pedimos al usuario que introduzca los dos números que quiere elevar a potencia
            IO.puts("Introduzca el primer número:")
            a = IO.gets("")

            IO.puts("Introduzca el segundo número:")
            b = IO.gets("")

            # Convertimos los números introducidos a enteros
            a = String.to_integer(a)
            b = String.to_integer(b)

            # Llamamos a la función "potencia" del módulo "OtroModulo" y guardamos el resultado en la variable "resultado"
            resultado = OtroModulo.potencia(a, b)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "6" ->
            # Pedimos al usuario que introduzca el número del que quiere calcular la raíz cuadrada
            IO.puts("Introduzca el número:")
            a = IO.gets("")

            # Convertimos el número introducido a entero
            a = String.to_integer(a)

            # Llamamos a la función "raiz" del módulo "OtroModulo" y guardamos el resultado en la variable "resultado"
            resultado = OtroModulo.raiz(a, 2)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "7" ->
            # Pedimos al usuario que introduzca los dos números que quiere calcular el logaritmo
            IO.puts("Introduzca el primer número:")
            a = IO.gets("")

            IO.puts("Introduzca el segundo número:")
            b = IO.gets("")

            # Convertimos los números introducidos a enteros
            a = String.to_integer(a)
            b = String.to_integer(b)

            # Llamamos a la función "logaritmo" del módulo "OtroModulo" y guardamos el resultado en la variable "resultado"
            resultado = OtroModulo.logaritmo(a, b)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "8" ->
            # Pedimos al usuario que introduzca el ángulo en grados del que quiere calcular el seno
            IO.puts("Introduzca el ángulo:")
            a = IO.gets("")

            # Convertimos el número introducido a entero
            a = String.to_integer(a)

            # Llamamos a la función "seno" del módulo "OtroModulo" y guardamos el resultado en la variable "resultado"
            resultado = OtroModulo.seno(a)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "9" ->
            # Pedimos al usuario que introduzca el ángulo en grados del que quiere calcular el coseno
            IO.puts("Introduzca el ángulo:")
            a = IO.gets("")

            # Convertimos el número introducido a entero
            a = String.to_integer(a)

            # Llamamos a la función "coseno" del módulo "OtroModulo" y guardamos el resultado en la variable "resultado"
            resultado = OtroModulo.coseno(a)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "10" ->
            # Pedimos al usuario que introduzca el ángulo en grados del que quiere calcular la tangente
            IO.puts("Introduzca el ángulo:")
            a = IO.gets("")

            # Convertimos el número introducido a entero
            a = String.to_integer(a)

            # Llamamos a la función "tangente" del módulo "OtroModulo" y guardamos el resultado en la variable "resultado"
            resultado = OtroModulo.tangente(a)

            # Imprimimos en pantalla el resultado
            IO.puts("El resultado es: #{resultado}")
          "11" ->
            # Si el usuario introduce la opción "11", salimos del bucle infinito y el programa termina