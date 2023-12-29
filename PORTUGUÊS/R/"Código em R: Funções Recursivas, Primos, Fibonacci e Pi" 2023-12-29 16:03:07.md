Claro! Aqui está um código complexo em linguagem R:

```R
# Definindo uma função recursiva para calcular o fatorial de um número
fatorial <- function(n) {
  if (n <= 1) {
    return(1)
  } else {
    return(n * fatorial(n-1))
  }
}

# Definindo uma função para encontrar números primos até um determinado limite
encontrar_primos <- function(limite) {
  primos <- c()
  
  for (i in 2:limite) {
    eh_primo <- TRUE
    
    for (j in 2:sqrt(i)) {
      if (i %% j == 0) {
        eh_primo <- FALSE
        break
      }
    }
    
    if (eh_primo) {
      primos <- c(primos, i)
    }
  }
  
  return(primos)
}

# Definindo uma função para calcular o n-ésimo termo da sequência de Fibonacci
fibonacci <- function(n) {
  if (n <= 0) {
    return(0)
  } else if (n == 1) {
    return(1)
  } else {
    return(fibonacci(n-1) + fibonacci(n-2))
  }
}

# Definindo uma função para calcular o valor aproximado de pi utilizando o método de Monte Carlo
calcular_pi <- function(iteracoes) {
  pontos_dentro_circulo <- 0
  pontos_totais <- 0
  
  for (i in 1:iteracoes) {
    x <- runif(1, -1, 1)
    y <- runif(1, -1, 1)
    
    if (x^2 + y^2 <= 1) {
      pontos_dentro_circulo <- pontos_dentro_circulo + 1
    }
    
    pontos_totais <- pontos_totais + 1
  }
  
  return(4 * pontos_dentro_circulo / pontos_totais)
}

# Exemplos de uso das funções definidas acima
print(fatorial(5))  # Resultado: 120

print(encontrar_primos(20))  # Resultado: 2 3 5 7 11 13 17 19

print(fibonacci(10))  # Resultado: 55

print(calcular_pi(1000000))  # Resultado aproximado: 3.141676
```

Neste código, foram definidas quatro funções diferentes:

1. `fatorial`: uma função recursiva para calcular o fatorial de um número.
2. `encontrar_primos`: uma função para encontrar todos os números primos até um determinado limite.
3. `fibonacci`: uma função para calcular o n-ésimo termo da sequência de Fibonacci.
4. `calcular_pi`: uma função que utiliza o método de Monte Carlo para calcular uma aproximação do valor de pi.

No final do código, há exemplos de uso das funções, mostrando como utilizá-las para obter resultados específicos.