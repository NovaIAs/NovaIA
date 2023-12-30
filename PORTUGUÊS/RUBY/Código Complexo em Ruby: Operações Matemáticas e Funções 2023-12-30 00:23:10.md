Claro! Aqui está um exemplo de um código complexo em Ruby que realiza uma série de operações matemáticas complexas:

```ruby
# Função para calcular o fatorial de um número
def fatorial(n)
  if n == 0 || n == 1
    return 1
  else
    return n * fatorial(n - 1)
  end
end

# Função para verificar se um número é primo
def primo?(n)
  return false if n <= 1

  (2..Math.sqrt(n)).each do |i|
    return false if n % i == 0
  end

  return true
end

# Função para calcular o número de Fibonacci
def fibonacci(n)
  return n if n <= 1

  fib = [0, 1]
  (2..n).each do |i|
    fib[i] = fib[i - 1] + fib[i - 2]
  end

  return fib[n]
end

# Função para gerar um número aleatório entre dois limites
def gerar_numero_aleatorio(min, max)
  return rand(min..max)
end

# Exemplo de utilização das funções
numero = gerar_numero_aleatorio(1, 100)
puts "Número aleatório gerado: #{numero}"

fatorial_numero = fatorial(numero)
puts "O fatorial de #{numero} é: #{fatorial_numero}"

if primo?(numero)
  puts "#{numero} é um número primo"
else
  puts "#{numero} não é um número primo"
end

fibonacci_numero = fibonacci(numero)
puts "O número de Fibonacci na posição #{numero} é: #{fibonacci_numero}"
```

Neste código, existem quatro funções principais:

1. A função `fatorial` recebe um número `n` como parâmetro e retorna o fatorial desse número.
2. A função `primo?` recebe um número `n` como parâmetro e retorna `true` se o número for primo, e `false` caso contrário.
3. A função `fibonacci` recebe um número `n` como parâmetro e retorna o número de Fibonacci na posição `n`.
4. A função `gerar_numero_aleatorio` recebe dois números `min` e `max` como parâmetros e retorna um número aleatório entre esses limites.

No exemplo de utilização das funções, um número aleatório é gerado utilizando a função `gerar_numero_aleatorio`. Em seguida, o fatorial desse número é calculado utilizando a função `fatorial`. Depois, é verificado se o número é primo utilizando a função `primo?`. Por fim, é calculado o número de Fibonacci na posição do número utilizando a função `fibonacci`. Os resultados são exibidos na saída.