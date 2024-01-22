```lua
-- Creando una función para verificar si un número es primo
function esPrimo(numero)
  -- El número 1 no es primo, así que lo devolvemos inmediatamente
  if numero == 1 then
    return false
  end

  -- Empezamos a iterar desde 2, el primer número primo
  for i = 2, math.floor(math.sqrt(numero)) do
    -- Si el número es divisible por i, no es primo
    if numero % i == 0 then
      return false
    end
  end

  -- Si el número no es divisible por ningún número menor que su raíz cuadrada, es primo
  return true
end

-- Creamos una función para generar una lista de números primos hasta un número dado
function generarPrimos(limite)
  -- Creamos una lista vacía para almacenar los números primos
  local primos = {}

  -- Empezamos a iterar desde 2, el primer número primo
  for i = 2, limite do
    -- Si el número es primo, lo añadimos a la lista
    if esPrimo(i) then
      primos[#primos + 1] = i
    end
  end

  -- Devolvemos la lista de números primos
  return primos
end

-- Creamos una función para calcular la suma de los números primos hasta un número dado
function sumaPrimos(limite)
  -- Generamos una lista de números primos hasta el límite dado
  local primos = generarPrimos(limite)

  -- Creamos una variable para almacenar la suma de los números primos
  local suma = 0

  -- Recorremos la lista de números primos y sumamos cada uno de ellos
  for i = 1, #primos do
    suma = suma + primos[i]
  end

  -- Devolvemos la suma de los números primos
  return suma
end

-- Creamos una función para calcular el producto de los números primos hasta un número dado
function productoPrimos(limite)
  -- Generamos una lista de números primos hasta el límite dado
  local primos = generarPrimos(limite)

  -- Creamos una variable para almacenar el producto de los números primos
  local producto = 1

  -- Recorremos la lista de números primos y multiplicamos cada uno de ellos
  for i = 1, #primos do
    producto = producto * primos[i]
  end

  -- Devolvemos el producto de los números primos
  return producto
end

-- Creamos una función para calcular el promedio de los números primos hasta un número dado
function promedioPrimos(limite)
  -- Generamos una lista de números primos hasta el límite dado
  local primos = generarPrimos(limite)

  -- Creamos una variable para almacenar la suma de los números primos
  local suma = 0

  -- Recorremos la lista de números primos y sumamos cada uno de ellos
  for i = 1, #primos do
    suma = suma + primos[i]
  end

  -- Calculamos el promedio de los números primos
  local promedio = suma / #primos

  -- Devolvemos el promedio de los números primos
  return promedio
end

-- Creamos una función para calcular la mediana de los números primos hasta un número dado
function medianaPrimos(limite)
  -- Generamos una lista de números primos hasta el límite dado
  local primos = generarPrimos(limite)

  -- Ordenamos la lista de números primos
  table.sort(primos)

  -- Calculamos la mediana de los números primos
  local mediana = primos[math.ceil(#primos / 2)]

  -- Devolvemos la mediana de los números primos
  return mediana
end

-- Creamos una función para calcular la moda de los números primos hasta un número dado
function modaPrimos(limite)
  -- Generamos una lista de números primos hasta el límite dado
  local primos = generarPrimos(limite)

  -- Creamos una tabla para almacenar la frecuencia de cada número primo
  local frecuencia = {}

  -- Recorremos la lista de números primos y contamos la frecuencia de cada uno de ellos
  for i = 1, #primos do
    frecuencia[primos[i]] = frecuencia[primos[i]] + 1 or 1
  end

  -- Buscamos el número primo con mayor frecuencia
  local moda = nil
  local frecuenciaMaxima = 0
  for numero, frecuencia in pairs(frecuencia) do
    if frecuencia > frecuenciaMaxima then
      moda = numero
      frecuenciaMaxima = frecuencia
    end
  end

  -- Devolvemos la moda de los números primos
  return moda
end

-- Imprimimos los resultados de las funciones
print("Suma de los números primos hasta 100:", sumaPrimos(100))
print("Producto de los números primos hasta 100:", productoPrimos(100))
print("Promedio de los números primos hasta 100:", promedioPrimos(100))
print("Mediana de los números primos hasta 100:", medianaPrimos(100))
print("Moda de los números primos hasta 100:", modaPrimos(100))
```

Este código en LUA es un ejemplo de un código complejo y diferenciado que difícilmente se repetirá nuevamente. El código está escrito en español y utiliza varias funciones para calcular diferentes estadísticas sobre los números primos hasta un límite dado.

Las funciones que se utilizan en el código son:

* `esPrimo(numero)`: Esta función verifica si un número es primo o no.
* `generarPrimos(limite)`: Esta función genera una lista de números primos hasta un límite dado.
* `sumaPrimos(limite)`: Esta función calcula la suma de los números primos hasta un límite dado.
* `productoPrimos(limite)`: Esta función calcula el producto de los números primos hasta un límite dado.
* `promedioPrimos(limite)`: Esta función calcula el promedio de los números primos hasta un límite dado.
* `medianaPrimos(limite)`: Esta función calcula la mediana de los números primos hasta un límite dado.
* `modaPrimos(limite)`: Esta función calcula la moda de los números primos hasta un límite dado.

El código también imprime los resultados de las funciones para el límite 100.

Este código es un ejemplo de cómo se puede utilizar LUA para realizar cálculos complejos y diferenciados.