```ruby
# Definimos una clase llamada "Calculadora"
class Calculadora
  # Creamos un método llamado "suma" que toma dos argumentos y los suma
  def suma(a, b)
    a + b
  end

  # Creamos un método llamado "resta" que toma dos argumentos y los resta
  def resta(a, b)
    a - b
  end

  # Creamos un método llamado "multiplicación" que toma dos argumentos y los multiplica
  def multiplicación(a, b)
    a * b
  end

  # Creamos un método llamado "división" que toma dos argumentos y los divide
  def división(a, b)
    a / b
  end

  # Creamos un método llamado "potencia" que toma dos argumentos y eleva el primero a la potencia del segundo
  def potencia(a, b)
    a ** b
  end

  # Creamos un método llamado "raíz cuadrada" que toma un argumento y calcula su raíz cuadrada
  def raíz_cuadrada(a)
    Math.sqrt(a)
  end

  # Creamos un método llamado "seno" que toma un argumento y calcula su seno
  def seno(a)
    Math.sin(a)
  end

  # Creamos un método llamado "coseno" que toma un argumento y calcula su coseno
  def coseno(a)
    Math.cos(a)
  end

  # Creamos un método llamado "tangente" que toma un argumento y calcula su tangente
  def tangente(a)
    Math.tan(a)
  end
end

# Creamos un objeto de la clase "Calculadora"
calculadora = Calculadora.new

# Utilizamos el objeto "calculadora" para realizar algunas operaciones
resultado_suma = calculadora.suma(10, 20)
resultado_resta = calculadora.resta(30, 15)
resultado_multiplicación = calculadora.multiplicación(5, 10)
resultado_división = calculadora.división(20, 5)
resultado_potencia = calculadora.potencia(2, 3)
resultado_raíz_cuadrada = calculadora.raíz_cuadrada(25)
resultado_seno = calculadora.seno(Math::PI / 2)
resultado_coseno = calculadora.coseno(Math::PI)
resultado_tangente = calculadora.tangente(Math::PI / 4)

# Mostramos los resultados de las operaciones realizadas
puts "Resultado de la suma: #{resultado_suma}"
puts "Resultado de la resta: #{resultado_resta}"
puts "Resultado de la multiplicación: #{resultado_multiplicación}"
puts "Resultado de la división: #{resultado_división}"
puts "Resultado de la potencia: #{resultado_potencia}"
puts "Resultado de la raíz cuadrada: #{resultado_raíz_cuadrada}"
puts "Resultado del seno: #{resultado_seno}"
puts "Resultado del coseno: #{resultado_coseno}"
puts "Resultado de la tangente: #{resultado_tangente}"
```

Explicación:

* Creamos una clase llamada "Calculadora" que contiene métodos para realizar operaciones aritméticas básicas y algunas funciones trigonométricas.
* Creamos un objeto de la clase "Calculadora" llamado "calculadora".
* Utilizamos el objeto "calculadora" para realizar algunas operaciones y almacenamos los resultados en variables.
* Mostramos los resultados de las operaciones realizadas en la consola.