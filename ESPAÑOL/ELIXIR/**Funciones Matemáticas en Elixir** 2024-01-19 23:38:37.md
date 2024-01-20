```elixir
# Definición de una función para obtener el factorial de un número
def factorial(n) when n == 0, do: 1
def factorial(n) when n > 0, do: n * factorial(n-1)

# Definición de una función para generar una lista de números primos hasta un límite
def primes_up_to(limit) do
  primes = [2]
  current = 3
  while current <= limit do
    is_prime = true
    for prime <- primes do
      if rem(current, prime) == 0 do
        is_prime = false
        break
      end
    end
    if is_prime do
      primes = [primes, current]
    end
    current = current + 2
  end
  primes
end

# Definición de una función para generar una lista de números perfectos hasta un límite
def perfect_numbers_up_to(limit) do
  perfect_numbers = []
  for i <- 2..limit do
    divisors = [1]
    for j <- 2..(i/2) do
      if rem(i, j) == 0 do
        divisors = [divisors, j]
      end
    end
    if sum(divisors) == i do
      perfect_numbers = [perfect_numbers, i]
    end
  end
  perfect_numbers
end

# Definición de una función para generar una lista de números de Fibonacci hasta un límite
def fibonacci_up_to(limit) do
  fibonacci = [0, 1]
  while last(fibonacci) <= limit do
    next_number = last(fibonacci) + penultimate(fibonacci)
    fibonacci = [fibonacci, next_number]
  end
  fibonacci
end

# Obtener el factorial de un número
IO.puts "El factorial de 5 es #{factorial(5)}"

# Obtener una lista de números primos hasta 100
IO.puts "Los números primos hasta 100 son #{primes_up_to(100)}"

# Obtener una lista de números perfectos hasta 1000
IO.puts "Los números perfectos hasta 1000 son #{perfect_numbers_up_to(1000)}"

# Obtener una lista de números de Fibonacci hasta 100
IO.puts "Los números de Fibonacci hasta 100 son #{fibonacci_up_to(100)}"
```

Este código Elixir realiza las siguientes tareas:

1. Define una función factorial que calcula el factorial de un número dado.
2. Define una función primes_up_to que genera una lista de números primos hasta un límite especificado.
3. Define una función perfect_numbers_up_to que genera una lista de números perfectos hasta un límite especificado.
4. Define una función fibonacci_up_to que genera una lista de números de Fibonacci hasta un límite especificado.
5. Utiliza las funciones definidas anteriormente para obtener y mostrar el factorial de un número, una lista de números primos hasta 100, una lista de números perfectos hasta 1000 y una lista de números de Fibonacci hasta 100.