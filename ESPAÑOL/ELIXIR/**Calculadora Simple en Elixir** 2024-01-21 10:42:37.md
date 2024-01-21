```elixir
# Definición de módulos

# Módulo para manejar la interfaz de usuario
defmodule Interfaz do
  def iniciar() do
    IO.puts "Bienvenido al programa."
  end

  def pedir_entrada(mensaje) do
    IO.write mensaje <> ": "
    :io.getn "" |> String.trim()
  end

  def mostrar_salida(mensaje) do
    IO.puts mensaje
  end
end

# Módulo para manejar el procesamiento de datos
defmodule Procesamiento do
  def calcular_suma(a, b) do
    a + b
  end

  def calcular_resta(a, b) do
    a - b
  end

  def calcular_multiplicacion(a, b) do
    a * b
  end

  def calcular_division(a, b) do
    a / b
  end
end

# Módulo principal
defmodule Principal do
  def main() do
    Interfaz.iniciar()

    operacion = Interfaz.pedir_entrada("Introduce la operación a realizar (suma, resta, multiplicacion, division): ")
    numero_1 = Interfaz.pedir_entrada("Introduce el primer número: ").to_integer()
    numero_2 = Interfaz.pedir_entrada("Introduce el segundo número: ").to_integer()

    resultado = Procesamiento.calcular_suma(numero_1, numero_2)
    if operacion == "suma" do
      Interfaz.mostrar_salida("La suma de los números es: #{resultado}")
    elseif operacion == "resta" do
      Interfaz.mostrar_salida("La resta de los números es: #{resultado}")
    elseif operacion == "multiplicacion" do
      Interfaz.mostrar_salida("La multiplicación de los números es: #{resultado}")
    elseif operacion == "division" do
      Interfaz.mostrar_salida("La división de los números es: #{resultado}")
    else
      Interfaz.mostrar_salida("Operación no reconocida.")
    end
  end
end

Principal.main()
```

Explicación del código:

* El código define tres módulos: `Interfaz`, `Procesamiento` y `Principal`.
* El módulo `Interfaz` se utiliza para manejar la interacción con el usuario, pidiendo entradas y mostrando salidas.
* El módulo `Procesamiento` se utiliza para realizar los cálculos matemáticos.
* El módulo `Principal` es el módulo principal que ejecuta el programa.
* La función `main/0` del módulo `Principal` inicia el programa, pide al usuario que introduzca la operación a realizar y los dos números, y luego llama a la función `calcular_suma/2` del módulo `Procesamiento` para realizar el cálculo.
* El resultado del cálculo se muestra al usuario utilizando la función `mostrar_salida/1` del módulo `Interfaz`.
* El código utiliza la sintaxis de Elixir, que es un lenguaje de programación funcional.