```lua
-- Librerías utilizadas.
local math = require "math"
local string = require "string"
local table = require "table"

-- Función para calcular el factorial de un número.
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end

-- Función para generar una lista de números primos hasta un límite especificado.
function prime_list(n)
  local primes = {}
  for i = 2, n do
    local is_prime = true
    for j = 2, math.floor(math.sqrt(i)) do
      if i % j == 0 then
        is_prime = false
        break
      end
    end
    if is_prime then
      primes[#primes + 1] = i
    end
  end
  return primes
end

-- Función para invertir una cadena de caracteres.
function reverse_string(s)
  local reversed = ""
  for i = #s, 1, -1 do
    reversed = reversed .. string.sub(s, i, i)
  end
  return reversed
end

-- Función para ordenar una tabla de números en orden ascendente.
function sort_table(t)
  table.sort(t, function(a, b) return a < b end)
  return t
end

-- Función principal.
function main()
  -- Calcular el factorial de 10.
  local factorial_10 = factorial(10)
  print("El factorial de 10 es:", factorial_10)

  -- Generar una lista de números primos hasta 100.
  local primes_100 = prime_list(100)
  print("Los números primos hasta 100 son:", primes_100)

  -- Invertir la cadena "Hola mundo".
  local reversed_string = reverse_string("Hola mundo")
  print("La cadena invertida es:", reversed_string)

  -- Ordenar una tabla de números en orden ascendente.
  local numbers = {5, 2, 8, 3, 1, 4, 7, 6, 9}
  local sorted_numbers = sort_table(numbers)
  print("La tabla ordenada es:", sorted_numbers)
end

-- Llamar a la función principal.
main()
```

Explicación del código:

* La función `factorial` calcula el factorial de un número utilizando recursividad.
* La función `prime_list` genera una lista de números primos hasta un límite especificado utilizando el algoritmo primo de Eratóstenes.
* La función `reverse_string` invierte una cadena de caracteres utilizando un bucle `for`.
* La función `sort_table` ordena una tabla de números en orden ascendente utilizando la función `table.sort`.
* La función `main` es la función principal del programa. Llama a las funciones anteriores para realizar varias tareas, como calcular el factorial de 10, generar una lista de números primos hasta 100, invertir la cadena "Hola mundo" y ordenar una tabla de números en orden ascendente.
* La función `main` es llamada al final del programa para ejecutar el programa.