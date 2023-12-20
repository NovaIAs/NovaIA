Claro! Aqui está um exemplo de um código complexo em R:

```R
# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * fatorial(n-1))
  }
}

# Função para verificar se um número é primo
eh_primo <- function(n) {
  if (n <= 1) {
    return(FALSE)
  } else {
    for (i in 2:sqrt(n)) {
      if (n %% i == 0) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
}

# Função para calcular o número de Fibonacci
fibonacci <- function(n) {
  if (n <= 0) {
    return(0)
  } else if (n == 1) {
    return(1)
  } else {
    return(fibonacci(n-1) + fibonacci(n-2))
  }
}

# Função para verificar se uma palavra é um palíndromo
eh_palindromo <- function(palavra) {
  palavra <- tolower(palavra)
  palavra_reversa <- strsplit(palavra, "")[[1]][length(palavra):1]
  palavra_reversa <- paste(palavra_reversa, collapse = "")
  if (palavra == palavra_reversa) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Função para calcular o quadrado de um número
quadrado <- function(n) {
  return(n^2)
}

# Função para calcular a média de uma lista de números
media <- function(lista) {
  soma <- sum(lista)
  qtd_elementos <- length(lista)
  return(soma/qtd_elementos)
}

# Função para calcular o desvio padrão de uma lista de números
desvio_padrao <- function(lista) {
  media_lista <- media(lista)
  soma_diferencas <- sum((lista - media_lista)^2)
  qtd_elementos <- length(lista)
  return(sqrt(soma_diferencas/qtd_elementos))
}

# Função para encontrar o valor máximo em uma lista de números
maximo <- function(lista) {
  return(max(lista))
}

# Função para encontrar o valor mínimo em uma lista de números
minimo <- function(lista) {
  return(min(lista))
}

# Função para realizar uma busca binária em uma lista ordenada
busca_binaria <- function(lista, valor) {
  inicio <- 1
  fim <- length(lista)
  
  while (inicio <= fim) {
    meio <- floor((inicio + fim)/2)
    
    if (lista[meio] == valor) {
      return(meio)
    }
    
    if (lista[meio] < valor) {
      inicio <- meio + 1
    } else {
      fim <- meio - 1
    }
  }
  
  return(-1)
}

# Função para calcular a raiz quadrada de um número utilizando o método de Newton
raiz_quadrada <- function(n) {
  x <- n
  while (abs(x^2 - n) > 0.00001) {
    x <- (x + n/x)/2
  }
  return(x)
}
```

Neste exemplo, forneci uma coleção de funções em R que realizam diferentes tarefas. Cada função tem um propósito específico, como calcular o fatorial de um número, verificar se um número é primo, calcular o número de Fibonacci, verificar se uma palavra é um palíndromo, calcular o quadrado de um número, calcular a média e desvio padrão de uma lista de números, encontrar o valor máximo e mínimo em uma lista de números, realizar uma busca binária em uma lista ordenada e calcular a raiz quadrada de um número utilizando o método de Newton.

Essas funções podem ser utilizadas em diferentes projetos e aplicações que envolvem cálculos matemáticos e manipulação de dados.