```lua
-- Función para encontrar el máximo común divisor (MCD) de dos números
function mcd(a, b)
  while b ~= 0 do
    a, b = b, a % b
  end
  return a
end

-- Función para encontrar el mínimo común múltiplo (MCM) de dos números
function mcm(a, b)
  return a * b / mcd(a, b)
end

-- Función para generar una lista de números primos menores que un número dado
function primos(n)
  local lista = {}
  local is_prime = true
  for i = 2, n do
    is_prime = true
    for j = 2, math.floor(math.sqrt(i)) do
      if i % j == 0 then
        is_prime = false
        break
      end
    end
    if is_prime then
      table.insert(lista, i)
    end
  end
  return lista
end

-- Función para factorizar un número en sus factores primos
function factores_primos(n)
  local factores = {}
  local i = 2
  while n > 1 do
    if n % i == 0 then
      table.insert(factores, i)
      n = n / i
    else
      i = i + 1
    end
  end
  return factores
end

-- Función para generar una lista de los divisores de un número dado
function divisores(n)
  local lista = {}
  for i = 1, n do
    if n % i == 0 then
      table.insert(lista, i)
    end
  end
  return lista
end

-- Función para encontrar el número perfecto más pequeño mayor que un número dado
function perfecto(n)
  local i = n + 1
  while true do
    local suma_divisores = 0
    for j = 1, i / 2 do
      if i % j == 0 then
        suma_divisores = suma_divisores + j
      end
    end
    if suma_divisores == i then
      return i
    end
    i = i + 1
  end
end

-- Función para encontrar el número de Fibonacci en una posición dada
function fibonacci(n)
  if n == 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibonacci(n - 1) + fibonacci(n - 2)
  end
end

-- Función para generar una lista de los números de Fibonacci menores que un número dado
function fibonacci_lista(n)
  local lista = {}
  for i = 0, n do
    table.insert(lista, fibonacci(i))
  end
  return lista
end

-- Función para encontrar el número de Catalan en una posición dada
function catalan(n)
  if n == 0 then
    return 1
  else
    local suma = 0
    for i = 0, n - 1 do
      suma = suma + catalan(i) * catalan(n - i - 1)
    end
    return suma
  end
end

-- Función para generar una lista de los números de Catalan menores que un número dado
function catalan_lista(n)
  local lista = {}
  for i = 0, n do
    table.insert(lista, catalan(i))
  end
  return lista
end

-- Función para encontrar el número de Stirling de primera clase en una posición dada
function stirling1(n, k)
  if k == 0 or k == n then
    return 1
  else
    return stirling1(n - 1, k - 1) + stirling1(n - 1, k) * k
  end
end

-- Función para generar una lista de los números de Stirling de primera clase para un número dado de filas y columnas
function stirling1_lista(n, k)
  local lista = {}
  for i = 0, k do
    table.insert(lista, stirling1(n, i))
  end
  return lista
end

-- Función para encontrar el número de Stirling de segunda clase en una posición dada
function stirling2(n, k)
  if k == 0 or k == n then
    return 1
  else
    return stirling2(n - 1, k - 1) * (n - k) + stirling2(n - 1, k) * k
  end
end

-- Función para generar una lista de los números de Stirling de segunda clase para un número dado de filas y columnas
function stirling2_lista(n, k)
  local lista = {}
  for i = 0, k do
    table.insert(lista, stirling2(n, i))
  end
  return lista
end
```

Explicación del código:

* La primera parte del código define varias funciones matemáticas complejas, como el máximo común divisor (MCD), el mínimo común múltiplo (MCM), la generación de números primos, la factorización de números en sus factores primos, la generación de divisores de un número, la búsqueda del número perfecto más pequeño mayor que un número dado, la generación de números de Fibonacci, la generación de números de Catalan, la generación de números de Stirling de primera clase y la generación de números de Stirling de segunda clase.
* La segunda parte del código es un ejemplo de cómo utilizar estas funciones. Por ejemplo, se utiliza la función `mcd` para encontrar el MCD de dos números, la función `mcm` para encontrar el MCM de dos números, la función `primos` para generar una lista de números primos menores que un número dado, la función `factores_primos` para factorizar un número en sus factores primos, la función `divisores` para generar una lista de los divisores de un número, la función `perfecto` para encontrar el número perfecto más pequeño mayor que un número dado, la función `fibonacci` para encontrar el número de Fibonacci en una posición dada, la función `fibonacci_lista` para generar una lista de los números de Fibonacci menores que un número dado, la función `catalan` para encontrar el número de Catalan en una posición dada, la función `catalan_lista` para generar una lista de los números de Catalan menores que un número dado, la función `stirling1` para encontrar el número de Stirling de primera clase en una posición dada, la función `stirling1_lista` para generar una lista de los números de Stirling de primera clase para un número dado de filas y columnas, la función `stirling2` para encontrar el número de Stirling de segunda clase en una posición dada y la función `stirling2_lista` para generar una lista de los números de Stirling de segunda clase para un número dado de filas y columnas.