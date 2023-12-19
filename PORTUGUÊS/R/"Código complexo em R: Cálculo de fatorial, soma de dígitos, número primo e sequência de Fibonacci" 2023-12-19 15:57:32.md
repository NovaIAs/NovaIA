Claro! Aqui está um código complexo em R:

```R
# Função para calcular o número fatorial de um número
calcular_fatorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * calcular_fatorial(n - 1))
  }
}

# Função para calcular a soma dos dígitos de um número
calcular_soma_digitos <- function(n) {
  soma <- 0
  while (n > 0) {
    soma <- soma + (n %% 10)
    n <- floor(n / 10)
  }
  return(soma)
}

# Função para verificar se um número é primo
verificar_numero_primo <- function(n) {
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

# Função para calcular a sequência de Fibonacci até um determinado limite
calcular_fibonacci <- function(limite) {
  fib <- c(0, 1)
  while (fib[length(fib)] + fib[length(fib) - 1] <= limite) {
    fib <- c(fib, fib[length(fib)] + fib[length(fib) - 1])
  }
  return(fib)
}

# Função principal
main <- function() {
  # Solicitar um número ao usuário
  numero <- as.integer(readline("Digite um número inteiro: "))
  
  # Calcular e exibir o fatorial do número
  fatorial <- calcular_fatorial(numero)
  cat("O fatorial de", numero, "é", fatorial, "\n")
  
  # Calcular e exibir a soma dos dígitos do número
  soma <- calcular_soma_digitos(numero)
  cat("A soma dos dígitos de", numero, "é", soma, "\n")
  
  # Verificar e exibir se o número é primo
  primo <- verificar_numero_primo(numero)
  if (primo) {
    cat(numero, "é um número primo\n")
  } else {
    cat(numero, "não é um número primo\n")
  }
  
  # Calcular e exibir a sequência de Fibonacci até o número
  fib <- calcular_fibonacci(numero)
  cat("A sequência de Fibonacci até", numero, "é:", paste(fib, collapse = ", "), "\n")
}

# Executar a função principal
main()
```

Esse código contém quatro funções diferentes:

1. A função `calcular_fatorial` recebe um número inteiro `n` como argumento e retorna o fatorial desse número.
2. A função `calcular_soma_digitos` recebe um número inteiro `n` como argumento e retorna a soma dos dígitos desse número.
3. A função `verificar_numero_primo` recebe um número inteiro `n` como argumento e retorna `TRUE` se o número for primo e `FALSE` caso contrário.
4. A função `calcular_fibonacci` recebe um número inteiro `limite` como argumento e retorna a sequência de Fibonacci até esse limite.

A função principal `main` solicita um número inteiro ao usuário, calcula e exibe o fatorial desse número, a soma dos dígitos, verifica se é primo e calcula a sequência de Fibonacci até esse número.