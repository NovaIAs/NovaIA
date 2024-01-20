```ruby
# Definir una clase llamada "Calculadora"
class Calculadora

  # Definir un método llamado "sumar" que recibe dos argumentos
  def sumar(a, b)
    # Retornar la suma de los dos argumentos
    return a + b
  end

  # Definir un método llamado "restar" que recibe dos argumentos
  def restar(a, b)
    # Retornar la resta del primer argumento menos el segundo argumento
    return a - b
  end

  # Definir un método llamado "multiplicar" que recibe dos argumentos
  def multiplicar(a, b)
    # Retornar la multiplicación de los dos argumentos
    return a * b
  end

  # Definir un método llamado "dividir" que recibe dos argumentos
  def dividir(a, b)
    # Verificar si el denominador (b) es igual a cero
    if b == 0
      # Si el denominador es cero, retornar un mensaje de error
      return "Error: El denominador no puede ser cero"
    else
      # Si el denominador no es cero, retornar el resultado de la división
      return a / b
    end
  end

  # Definir un método llamado "potencia" que recibe dos argumentos
  def potencia(a, b)
    # Retornar la potencia del primer argumento elevado al segundo argumento
    return a ** b
  end

  # Definir un método llamado "raiz_cuadrada" que recibe un argumento
  def raiz_cuadrada(a)
    # Verificar si el argumento es negativo
    if a < 0
      # Si el argumento es negativo, retornar un mensaje de error
      return "Error: El argumento no puede ser negativo"
    else
      # Si el argumento no es negativo, retornar la raíz cuadrada del argumento
      return Math.sqrt(a)
    end
  end

end

# Crear un objeto de la clase "Calculadora"
calculadora = Calculadora.new

# Utilizar el método "sumar" para sumar dos números
resultado_suma = calculadora.sumar(10, 5)

# Utilizar el método "restar" para restar dos números
resultado_resta = calculadora.restar(10, 5)

# Utilizar el método "multiplicar" para multiplicar dos números
resultado_multiplicacion = calculadora.multiplicar(10, 5)

# Utilizar el método "dividir" para dividir dos números
resultado_division = calculadora.dividir(10, 5)

# Utilizar el método "potencia" para elevar un número a una potencia
resultado_potencia = calculadora.potencia(10, 2)

# Utilizar el método "raiz_cuadrada" para calcular la raíz cuadrada de un número
resultado_raiz_cuadrada = calculadora.raiz_cuadrada(9)

# Imprimir los resultados en la consola
puts "Resultado de la suma: #{resultado_suma}"
puts "Resultado de la resta: #{resultado_resta}"
puts "Resultado de la multiplicación: #{resultado_multiplicacion}"
puts "Resultado de la división: #{resultado_division}"
puts "Resultado de la potencia: #{resultado_potencia}"
puts "Resultado de la raíz cuadrada: #{resultado_raiz_cuadrada}"
```

Explicación del código:

* La primera línea define una clase llamada "Calculadora".
* Las siguientes líneas definen los métodos de la clase "Calculadora": "sumar", "restar", "multiplicar", "dividir", "potencia" y "raiz_cuadrada".
* Cada método recibe los argumentos necesarios para realizar la operación correspondiente y retorna el resultado de la operación.
* La última línea crea un objeto de la clase "Calculadora" llamado "calculadora".
* Las siguientes líneas utilizan los métodos del objeto "calculadora" para realizar las operaciones correspondientes y almacenar los resultados en variables.
* La última línea imprime los resultados en la consola.