```lua
-- Función para encontrar el factorial de un número entero no negativo.
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Función para encontrar el máximo común divisor de dos números enteros positivos.
function mcd(a, b)
  if b == 0 then
    return a
  else
    return mcd(b, a % b)
  end
end

-- Función para encontrar el mínimo común múltiplo de dos números enteros positivos.
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Función para encontrar el menor número positivo que es divisible por todos los números del 1 al n.
function mcm_hasta_n(n)
  local mcm = 1
  for i = 1, n do
    mcm = mcm(mcm, i)
  end
  return mcm
end

-- Función para encontrar la suma de los dígitos de un número entero positivo.
function suma_digitos(n)
  if n < 10 then
    return n
  else
    return (n % 10) + suma_digitos(n / 10)
  end
end

-- Función para encontrar el número de dígitos de un número entero positivo.
function num_digitos(n)
  if n < 10 then
    return 1
  else
    return 1 + num_digitos(n / 10)
  end
end

-- Función para encontrar el número de divisores de un número entero positivo.
function num_divisores(n)
  local divisores = 0
  for i = 1, n do
    if n % i == 0 then
      divisores = divisores + 1
    end
  end
  return divisores
end

-- Función para encontrar el número de divisores primos de un número entero positivo.
function num_divisores_primos(n)
  local divisores = 0
  for i = 2, n do
    if n % i == 0 and es_primo(i) then
      divisores = divisores + 1
    end
  end
  return divisores
end

-- Función para comprobar si un número entero positivo es primo.
function es_primo(n)
  if n < 2 then
    return false
  end
  for i = 2, math.floor(math.sqrt(n)) do
    if n % i == 0 then
      return false
    end
  end
  return true
end

-- Función para encontrar el mayor factor primo de un número entero positivo.
function mayor_factor_primo(n)
  local mayor_primo = 1
  for i = 2, n do
    if n % i == 0 and es_primo(i) then
      mayor_primo = i
    end
  end
  return mayor_primo
end

-- Función para descomponer un número entero positivo en sus factores primos.
function factores_primos(n)
  local factores = {}
  for i = 2, n do
    while n % i == 0 do
      factores[#factores + 1] = i
      n = n / i
    end
  end
  return factores
end

-- Función para encontrar el número de cuadrados perfectos en un rango de números enteros positivos.
function num_cuadrados_perfectos(a, b)
  local cuadrados = 0
  for i = a, b do
    if math.sqrt(i) == math.floor(math.sqrt(i)) then
      cuadrados = cuadrados + 1
    end
  end
  return cuadrados
end

-- Función para encontrar el número de números primos en un rango de números enteros positivos.
function num_primos(a, b)
  local primos = 0
  for i = a, b do
    if es_primo(i) then
      primos = primos + 1
    end
  end
  return primos
end

-- Función para encontrar el número de números perfectos en un rango de números enteros positivos.
function num_perfectos(a, b)
  local perfectos = 0
  for i = a, b do
    if es_perfecto(i) then
      perfectos = perfectos + 1
    end
  end
  return perfectos
end

-- Función para comprobar si un número entero positivo es perfecto.
function es_perfecto(n)
  local divisores = 0
  for i = 1, math.floor(n / 2) do
    if n % i == 0 then
      divisores = divisores + i
    end
  end
  return divisores == n
end
```

Explicación del código:

* La primera parte del código define varias funciones matemáticas comunes, como el factorial, el máximo común divisor, el mínimo común múltiplo, la suma de los dígitos, el número de dígitos, el número de divisores, el número de divisores primos, el mayor factor primo, la descomposición en factores primos, el número de cuadrados perfectos, el número de números primos y el número de números perfectos en un rango de números enteros positivos.
* La segunda parte del código define una función para comprobar si un número entero positivo es perfecto.
* La tercera parte del código define una función para encontrar el factorial de un número entero no negativo.
* La cuarta parte del código define una función para encontrar el máximo común divisor de dos números enteros positivos.
* La quinta parte del código define una función para encontrar el mínimo común múltiplo de dos números enteros positivos.
* La sexta parte del código define una función para encontrar el menor número positivo que es divisible por todos los números del 1 al n.
* La séptima parte del código define una función para encontrar la suma de los dígitos de un número entero positivo.
* La octava parte del código define una función para encontrar el número de dígitos de un número entero positivo.
* La novena parte del código define una función para encontrar el número de divisores de un número entero positivo.
* La décima parte del código define una función para encontrar el número de divisores primos de un número entero positivo.
* La undécima parte del código define una función para comprobar si un número entero positivo es primo.
* La duodécima parte del código define una función para encontrar el mayor factor primo de un número entero positivo.
* La decimotercera parte del código define una función para descomponer un número entero positivo en sus factores primos.
* La decimocuarta parte del código define una función para encontrar el número de cuadrados perfectos en un rango de números enteros positivos.
* La decimoquinta parte del código define una función para encontrar el número de números primos en un rango de números enteros positivos.
* La decimosexta parte del código define una función para encontrar el número de números perfectos en un rango de números enteros positivos.