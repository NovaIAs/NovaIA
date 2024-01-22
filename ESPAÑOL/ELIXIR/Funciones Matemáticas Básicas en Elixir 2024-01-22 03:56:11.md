```elixir
# Función para encontrar el máximo común divisor (MCD) de dos números
def gcd(a, b) do
  if b == 0 do
    a
  else
    gcd(b, rem(a, b))
  end
end

# Función para encontrar el mínimo común múltiplo (MCM) de dos números
def lcm(a, b) do
  (a * b) div gcd(a, b)
end

# Función para encontrar el factorial de un número
def factorial(n) do
  if n == 0 do
    1
  else
    n * factorial(n-1)
  end
end

# Función para encontrar el número de combinaciones de n elementos tomados de k en k
def combinations(n, k) do
  factorial(n) div (factorial(k) * factorial(n-k))
end

# Función para encontrar el número de permutaciones de n elementos tomados de k en k
def permutations(n, k) do
  factorial(n) div factorial(n-k)
end

# Función para generar una lista de números primos hasta un número dado
def prime_list(n) do
  primes = [2]
  for i <- 3..n do
    is_prime = true
    for prime in primes do
      if rem(i, prime) == 0 do
        is_prime = false
        break
      end
    end
    if is_prime do
      primes = [i | primes]
    end
  end
  primes
end

# Función para encontrar el número de divisores de un número
def num_divisors(n) do
  divisors = 0
  for i <- 1..n do
    if rem(n, i) == 0 do
      divisors = divisors + 1
    end
  end
  divisors
end

# Función para encontrar la suma de los divisores de un número
def sum_divisors(n) do
  sum = 0
  for i <- 1..n do
    if rem(n, i) == 0 do
      sum = sum + i
    end
  end
  sum
end

# Función para encontrar el número perfecto más pequeño mayor que un número dado
def smallest_perfect_number(n) do
  perfect_number = n + 1
  while sum_divisors(perfect_number) != 2 * perfect_number do
    perfect_number = perfect_number + 1
  end
  perfect_number
end
```

Explicación del código:

* La función `gcd` calcula el máximo común divisor (MCD) de dos números utilizando el algoritmo de Euclides.
* La función `lcm` calcula el mínimo común múltiplo (MCM) de dos números utilizando la fórmula \(MCM(a, b) = (a * b) / MCD(a, b)\).
* La función `factorial` calcula el factorial de un número utilizando la recursión.
* La función `combinations` calcula el número de combinaciones de \(n\) elementos tomados de \(k\) en \(k\).
* La función `permutations` calcula el número de permutaciones de \(n\) elementos tomados de \(k\) en \(k\).
* La función `prime_list` genera una lista de números primos hasta un número dado utilizando el algoritmo de la criba de Eratóstenes.
* La función `num_divisors` calcula el número de divisores de un número.
* La función `sum_divisors` calcula la suma de los divisores de un número.
* La función `smallest_perfect_number` encuentra el número perfecto más pequeño mayor que un número dado. Un número perfecto es un número que es igual a la suma de sus propios divisores propios (excepto el propio número).