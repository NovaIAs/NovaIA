Claro! Aqui está um código complexo em R:

```R
# Definindo uma função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * fatorial(n - 1))
  }
}

# Definindo uma função para verificar se um número é primo
is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Definindo uma função para gerar uma sequência de Fibonacci
fibonacci <- function(n) {
  fib_seq <- c(0, 1)
  if (n <= 2) {
    return(fib_seq[1:n])
  }
  for (i in 3:n) {
    fib_seq <- c(fib_seq, fib_seq[i-1] + fib_seq[i-2])
  }
  return(fib_seq)
}

# Definindo uma função para encontrar os números perfeitos em um intervalo
num_perfeitos <- function(start, end) {
  perfeitos <- c()
  for (n in start:end) {
    soma_divisores <- sum(divisors(n))
    if (soma_divisores == n) {
      perfeitos <- c(perfeitos, n)
    }
  }
  return(perfeitos)
}

# Testando as funções criadas
numero <- 5
print(paste("O fatorial de", numero, "é", fatorial(numero)))

numero <- 7
if (is_prime(numero)) {
  print(paste(numero, "é um número primo"))
} else {
  print(paste(numero, "não é um número primo"))
}

numero <- 10
print(paste("A sequência de Fibonacci com", numero, "números é:", paste(fibonacci(numero), collapse = ", ")))

inicio <- 1
fim <- 100
print(paste("Os números perfeitos entre", inicio, "e", fim, "são:", paste(num_perfeitos(inicio, fim), collapse = ", ")))
```

Neste código em R, foram definidas quatro funções:

1. `fatorial`: Calcula o fatorial de um número usando recursão.
2. `is_prime`: Verifica se um número é primo ou não.
3. `fibonacci`: Gera uma sequência de Fibonacci com base em um número fornecido.
4. `num_perfeitos`: Encontra os números perfeitos em um intervalo especificado.

Em seguida, as funções são testadas para alguns valores e os resultados são impressos no console. O código pode ser usado como referência para realizar cálculos matemáticos ou encontrar números especiais em um intervalo dado.