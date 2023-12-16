Claro! Aqui está um código complexo em R:

```R
# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n < 0) {
    stop("Erro: Não é possível calcular o fatorial de um número negativo.")
  } else if (n == 0 || n == 1) {
    return(1)
  } else {
    resultado <- 1
    for (i in 2:n) {
      resultado <- resultado * i
    }
    return(resultado)
  }
}

# Função para calcular a sequência de Fibonacci
fibonacci <- function(n) {
  if (n < 0) {
    stop("Erro: Não é possível calcular a sequência de Fibonacci para um número negativo.")
  } else if (n == 0) {
    return(0)
  } else if (n == 1 || n == 2) {
    return(1)
  } else {
    a <- 1
    b <- 1
    resultado <- c(a, b)
    for (i in 3:n) {
      proximo <- a + b
      resultado <- c(resultado, proximo)
      a <- b
      b <- proximo
    }
    return(resultado)
  }
}

# Função para verificar se um número é primo
primo <- function(n) {
  if (n < 2) {
    return(FALSE)
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Função para calcular o quadrado de um número
quadrado <- function(n) {
  return(n^2)
}

# Função para verificar se um número é par
par <- function(n) {
  if (n %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Função principal
main <- function() {
  # Solicitar um número ao usuário
  numero <- as.numeric(readline("Digite um número: "))
  
  # Calcular e exibir o fatorial do número
  cat("O fatorial de", numero, "é", fatorial(numero), "\n")
  
  # Calcular e exibir a sequência de Fibonacci até o número informado
  fib <- fibonacci(numero)
  cat("A sequência de Fibonacci até", numero, "é", paste(fib, collapse = ", "), "\n")
  
  # Verificar se o número é primo
  if (primo(numero)) {
    cat(numero, "é um número primo.\n")
  } else {
    cat(numero, "não é um número primo.\n")
  }
  
  # Calcular e exibir o quadrado do número
  cat("O quadrado de", numero, "é", quadrado(numero), "\n")
  
  # Verificar se o número é par
  if (par(numero)) {
    cat(numero, "é um número par.\n")
  } else {
    cat(numero, "é um número ímpar.\n")
  }
}

# Chamar a função principal
main()
```

Nesse código em R, temos uma função para calcular o fatorial de um número, uma função para calcular a sequência de Fibonacci, uma função para verificar se um número é primo, uma função para calcular o quadrado de um número e uma função para verificar se um número é par. A função principal solicita um número ao usuário, calcula e exibe o fatorial do número, a sequência de Fibonacci até o número informado, verifica se o número é primo, calcula e exibe o quadrado do número e verifica se o número é par ou ímpar.