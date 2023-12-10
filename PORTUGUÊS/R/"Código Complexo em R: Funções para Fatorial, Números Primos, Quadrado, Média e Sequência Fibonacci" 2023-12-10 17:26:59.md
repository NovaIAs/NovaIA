Claro! Aqui está um código complexo em R:

```R
# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if(n == 0) {
    return(1)
  } else {
    return(n * fatorial(n-1))
  }
}

# Função para verificar se um número é primo
is_primo <- function(n) {
  if(n < 2) {
    return(FALSE)
  } else {
    for(i in 2:sqrt(n)) {
      if(n %% i == 0) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Função para gerar uma lista de números primos
gerar_lista_primos <- function(n) {
  lista_primos <- c()
  i <- 2
  while(length(lista_primos) < n) {
    if(is_primo(i)) {
      lista_primos <- c(lista_primos, i)
    }
    i <- i + 1
  }
  return(lista_primos)
}

# Função para calcular o quadrado de um número
quadrado <- function(n) {
  return(n^2)
}

# Função para calcular a média de uma lista de números
calcular_media <- function(lista) {
  soma <- sum(lista)
  media <- soma / length(lista)
  return(media)
}

# Função para gerar uma sequência de Fibonacci
fibonacci <- function(n) {
  if(n <= 0) {
    return(NULL)
  }
  if(n == 1) {
    return(0)
  }
  if(n == 2) {
    return(1)
  }
  seq_fibonacci <- c(0, 1)
  for(i in 3:n) {
    seq_fibonacci <- c(seq_fibonacci, seq_fibonacci[i-1] + seq_fibonacci[i-2])
  }
  return(seq_fibonacci)
}

# Testando as funções
fatorial_5 <- fatorial(5)
print(paste("O fatorial de 5 é", fatorial_5))

lista_primos_10 <- gerar_lista_primos(10)
print(paste("Os primeiros 10 números primos são", paste(lista_primos_10, collapse = ", ")))

quadrado_7 <- quadrado(7)
print(paste("O quadrado de 7 é", quadrado_7))

media_lista <- calcular_media(c(1, 2, 3, 4, 5))
print(paste("A média da lista [1, 2, 3, 4, 5] é", media_lista))

seq_fibonacci_10 <- fibonacci(10)
print(paste("Os primeiros 10 números da sequência de Fibonacci são", paste(seq_fibonacci_10, collapse = ", ")))
```

Neste código, criei várias funções em R para realizar diferentes operações. A função `fatorial` calcula o fatorial de um número, a função `is_primo` verifica se um número é primo, a função `gerar_lista_primos` gera uma lista dos primeiros números primos, a função `quadrado` calcula o quadrado de um número, a função `calcular_media` calcula a média de uma lista de números e a função `fibonacci` gera uma sequência de Fibonacci.

No final do código, faço testes das funções para demonstrar seu funcionamento. Imprimo o fatorial de 5, os primeiros 10 números primos, o quadrado de 7, a média da lista [1, 2, 3, 4, 5] e os primeiros 10 números da sequência de Fibonacci.