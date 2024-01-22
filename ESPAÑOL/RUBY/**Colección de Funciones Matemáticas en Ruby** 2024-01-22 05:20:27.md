```ruby
# Función para calcular el máximo común divisor (MCD) de dos números
def mcd(a, b)
  return b if a == 0
  mcd(b % a, a)
end

# Función para calcular el mínimo común múltiplo (MCM) de dos números
def mcm(a, b)
  (a * b) / mcd(a, b)
end

# Función para generar una lista de números primos hasta un número dado
def primos_hasta(n)
  primos = []
  (2..n).each do |i|
    if (2..Math.sqrt(i)).all? { |j| i % j != 0 }
      primos.push(i)
    end
  end
  primos
end

# Función para factorizar un número en sus factores primos
def factores_primos(n)
  primos = primos_hasta(n)
  factores = []
  i = 0
  while n > 1
    if n % primos[i] == 0
      factores.push(primos[i])
      n /= primos[i]
    else
      i += 1
    end
  end
  factores
end

# Función para calcular la suma de los divisores propios de un número
def suma_divisores_propios(n)
  factores = factores_primos(n)
  suma = 1
  factores.each_with_index do |primo, i|
    (1..factores.count(primo)).each do |j|
      suma *= (primo ** j) + 1
    end
  end
  suma - n
end

# Función para calcular la cantidad de divisores de un número
def cantidad_divisores(n)
  factores = factores_primos(n)
  cantidad = 1
  factores.each do |primo|
    cantidad *= factores.count(primo) + 1
  end
  cantidad
end

# Función para calcular la suma de los cuadrados de los divisores de un número
def suma_cuadrados_divisores(n)
  factores = factores_primos(n)
  suma = 0
  factores.each do |primo|
    (1..factores.count(primo)).each do |j|
      suma += (primo ** (2 * j))
    end
  end
  suma
end

# Función para calcular la cantidad de divisores primos de un número
def cantidad_divisores_primos(n)
  factores = factores_primos(n)
  cantidad = 0
  factores.each do |primo|
    cantidad += 1 if primo.prime?
  end
  cantidad
end

# Función para calcular la suma de los recíprocos de los divisores de un número
def suma_reciprocos_divisores(n)
  factores = factores_primos(n)
  suma = 0
  factores.each do |primo|
    (1..factores.count(primo)).each do |j|
      suma += 1.0 / (primo ** j)
    end
  end
  suma
end

# Función para calcular la suma de los cuadrados de los recíprocos de los divisores de un número
def suma_cuadrados_reciprocos_divisores(n)
  factores = factores_primos(n)
  suma = 0
  factores.each do |primo|
    (1..factores.count(primo)).each do |j|
      suma += 1.0 / (primo ** (2 * j))
    end
  end
  suma
end

# Función para calcular la suma de las potencias de los divisores de un número
def suma_potencias_divisores(n, k)
  factores = factores_primos(n)
  suma = 0
  factores.each do |primo|
    (1..factores.count(primo)).each do |j|
      suma += (primo ** (j * k))
    end
  end
  suma
end
```

Este código en RUBY es una colección de funciones matemáticas que realizan operaciones complejas con números. Algunas de las funciones más importantes incluyen:

* `mcd`: Calcula el máximo común divisor de dos números.
* `mcm`: Calcula el mínimo común múltiplo de dos números.
* `primos_hasta`: Genera una lista de números primos hasta un número dado.
* `factores_primos`: Factoriza un número en sus factores primos.
* `suma_divisores_propios`: Calcula la suma de los divisores propios de un número.
* `cantidad_divisores`: Calcula la cantidad de divisores de un número.
* `suma_cuadrados_divisores`: Calcula la suma de los cuadrados de los divisores de un número.
* `cantidad_divisores_primos`: Calcula la cantidad de divisores primos de un número.
* `suma_reciprocos_divisores`: Calcula la suma de los recíprocos de los divisores de un número.
* `suma_cuadrados_reciprocos_divisores`: Calcula la suma de los cuadrados de los recíprocos de los divisores de un número.
* `suma_potencias_divisores`: Calcula la suma de las potencias de los divisores de un número.

Estas funciones se pueden utilizar para realizar una variedad de tareas matemáticas complejas, como encontrar el mínimo común múltiplo de dos números, encontrar los factores primos de un número, calcular la suma de los divisores propios de un número, etc.