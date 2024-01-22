```lua
-- Definición de la función factorial
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end

-- Definición de la función divisores
function divisores(n)
  local divisores = {}
  for i = 1, n do
    if n % i == 0 then
      table.insert(divisores, i)
    end
  end
  return divisores
end

-- Función para encontrar el máximo común divisor de dos números
function mcd(a, b)
  if b == 0 then
    return a
  else
    return mcd(b, a % b)
  end
end

-- Función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b)
  return (a * b) / mcd(a, b)
end

-- Función para determinar si un número es primo
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

-- Función para encontrar todos los números primos menores que un número dado
function primos(n)
  local primos = {}
  for i = 2, n do
    if es_primo(i) then
      table.insert(primos, i)
    end
  end
  return primos
end

-- Función para encontrar el mayor factor primo de un número
function mayor_factor_primo(n)
  local mayor_factor_primo = 1
  for i = 2, n do
    while n % i == 0 do
      mayor_factor_primo = i
      n = n / i
    end
  end
  return mayor_factor_primo
end

```

Este código incluye varias funciones matemáticas útiles:

* `factorial`: Calcula el factorial de un número.
* `divisores`: Devuelve todos los divisores de un número.
* `mcd`: Calcula el máximo común divisor de dos números.
* `mcm`: Calcula el mínimo común múltiplo de dos números.
* `es_primo`: Determina si un número es primo.
* `primos`: Devuelve todos los números primos menores que un número dado.
* `mayor_factor_primo`: Devuelve el mayor factor primo de un número.