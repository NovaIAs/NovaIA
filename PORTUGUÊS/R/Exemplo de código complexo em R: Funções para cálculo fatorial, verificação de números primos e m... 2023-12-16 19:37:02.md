Claro! Aqui está um exemplo de código complexo em R:

```
# Função para calcular o fatorial de um número
calcular_fatorial <- function(n) {
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * calcular_fatorial(n - 1))
  }
}

# Função para verificar se um número é primo
verificar_primo <- function(n) {
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

# Função para calcular a média de uma lista de números
calcular_media <- function(lista) {
  soma <- sum(lista)
  n <- length(lista)
  
  if (n == 0) {
    return(0)
  } else {
    return(soma / n)
  }
}

# Variáveis de exemplo
vetor <- c(1, 2, 3, 4, 5)
numero <- 7

# Chamando as funções
fatorial <- calcular_fatorial(numero)
is_primo <- verificar_primo(numero)
media <- calcular_media(vetor)

# Imprimindo os resultados
cat("O fatorial de", numero, "é", fatorial, "\n")
cat("O número", numero, "é primo?", is_primo, "\n")
cat("A média do vetor", vetor, "é", media, "\n")
```

Neste exemplo, temos três funções em R: `calcular_fatorial`, `verificar_primo` e `calcular_media`.
A função `calcular_fatorial` recebe um número `n` e calcula o fatorial dele utilizando recursão.
A função `verificar_primo` recebe um número `n` e verifica se ele é primo.
A função `calcular_media` recebe uma lista de números e calcula a média deles.

Em seguida, temos algumas variáveis de exemplo: `vetor`, que é um vetor com números inteiros, e `numero`, que é um número inteiro.

Depois disso, chamamos as funções com as variáveis de exemplo e armazenamos os resultados nas variáveis `fatorial`, `is_primo` e `media`.

Por fim, imprimimos os resultados utilizando a função `cat`.

Espero que isso seja útil para você! Se tiver alguma dúvida adicional, é só me perguntar.