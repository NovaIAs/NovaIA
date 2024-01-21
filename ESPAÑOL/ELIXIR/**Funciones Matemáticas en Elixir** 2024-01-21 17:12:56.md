```elixir
# Definir una función para calcular el factorial de un número
def factorial(n) do
  if n == 0, do: 1, else: n * factorial(n - 1)
end

# Definir una función para generar una lista de números primos
def prime_list(n) do
  Enum.filter(Enum.to_list(2..n), &is_prime/1)
end

# Definir una función para comprobar si un número es primo
def is_prime(n) do
  if n <= 1, do: false, else: Enum.all?(2..Integer.floor(Float.sqrt(n)), &rem(n, &1) != 0)
end

# Definir una función para generar una lista de números Fibonacci
def fibonacci_list(n) do
  Enum.take(Enum.reduce([0, 1], fn x, _ -> [Enum.at(x, 1) + Enum.at(x, 0) | x] end), n)
end

# Definir una función para calcular el máximo común divisor de dos números
def gcd(a, b) do
  if b == 0, do: a, else: gcd(b, rem(a, b))
end

# Definir una función para calcular el mínimo común múltiplo de dos números
def lcm(a, b) do
  a * b div gcd(a, b)
end

# Definir una función para generar una lista de números perfectos
def perfect_list(n) do
  Enum.filter(Enum.to_list(1..n), &is_perfect/1)
end

# Definir una función para comprobar si un número es perfecto
def is_perfect(n) do
  if n <= 1, do: false, else: Enum.sum(Enum.filter(1..Integer.floor(Float.sqrt(n)), &rem(n, &1) == 0)) == n
end

# Definir una función para generar una lista de números primos gemelos
def twin_prime_list(n) do
  Enum.filter(prime_list(n), &is_twin_prime/1)
end

# Definir una función para comprobar si un número primo es gemelo
def is_twin_prime(n) do
  Enum.member?(prime_list(n), n + 2)
end

# Definir una función para generar una lista de números primos de Mersenne
def mersenne_prime_list(n) do
  Enum.filter(Enum.to_list(2..n), &is_mersenne_prime/1)
end

# Definir una función para comprobar si un número primo es de Mersenne
def is_mersenne_prime(n) do
  if n < 2, do: false, else: Float.power(2, n) - 1 |> is_prime
end

# Definir una función para generar una lista de números primos de Sophie Germain
def sophie_germain_prime_list(n) do
  Enum.filter(prime_list(n), &is_sophie_germain_prime/1)
end

# Definir una función para comprobar si un número primo es de Sophie Germain
def is_sophie_germain_prime(n) do
  if n < 2, do: false, else: 2 * n + 1 |> is_prime
end

# Definir una función para generar una lista de números primos de Cunningham
def cunningham_prime_list(n) do
  Enum.filter(prime_list(n), &is_cunningham_prime/1)
end

# Definir una función para comprobar si un número primo es de Cunningham
def is_cunningham_prime(n) do
  if n < 2, do: false, else: (n + 1) * n - 1 |> is_prime
end
```

Explicación:

* La función `factorial` calcula el factorial de un número utilizando recursión.
* La función `prime_list` genera una lista de números primos hasta un número dado.
* La función `is_prime` comprueba si un número es primo.
* La función `fibonacci_list` genera una lista de números Fibonacci hasta un número dado.
* La función `gcd` calcula el máximo común divisor de dos números.
* La función `lcm` calcula el mínimo común múltiplo de dos números.
* La función `perfect_list` genera una lista de números perfectos hasta un número dado.
* La función `is_perfect` comprueba si un número es perfecto.
* La función `twin_prime_list` genera una lista de números primos gemelos hasta un número dado.
* La función `is_twin_prime` comprueba si un número primo es gemelo.
* La función `mersenne_prime_list` genera una lista de números primos de Mersenne hasta un número dado.
* La función `is_mersenne_prime` comprueba si un número primo es de Mersenne.
* La función `sophie_germain_prime_list` genera una lista de números primos de Sophie Germain hasta un número dado.
* La función `is_sophie_germain_prime` comprueba si un número primo es de Sophie Germain.
* La función `cunningham_prime_list` genera una lista de números primos de Cunningham hasta un número dado.
* La función `is_cunningham_prime` comprueba si un número primo es de Cunningham.